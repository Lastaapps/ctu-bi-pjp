use core::slice::SlicePattern;
use std::{collections::{HashMap, HashSet}, any::Any};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionOpcode, InstructionValue,
        IntValue, PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    ast::{Constant, Declaration, Expr, Function, Kind, Program, Statement, Value, Variable, Scope},
    base::Outcome,
    errors::MilaErr,
    tokens::BuiltInType,
};

struct VarSymbol<'a> {
    kind: Kind,
    ptr: PointerValue<'a>,
    is_const: bool,
}

struct FunSymbol<'a> {
    declaration: Declaration,
    value: FunctionValue<'a>,
}

struct LLVM<'a> {
    ctx: &'a Context,
    mdl: Module<'a>,
    bld: Builder<'a>,
    address_space: AddressSpace,
}

struct SymbolTable<'a> {
    symbols: Vec<HashMap<String, VarSymbol<'a>>>,
    functions: HashMap<String, FunSymbol<'a>>,
}

struct LocalCtx {
    // function name
    // return type
    // code block
}

enum ValuePair<'a> {
    IntPair(IntValue<'a>, IntValue<'a>),
    FloatPair(FloatValue<'a>, FloatValue<'a>),
}

impl BuiltInType {
    fn fn_name(&self) -> String {
        match self {
            BuiltInType::ReadLine => "built_read_line",
            BuiltInType::Write => "built_write",
            _ => panic!("Built name not supported for {self:?}"),
        }.to_string()
    }
}

impl Kind {
    fn mangle_name(&self) -> String {
        match self {
            Kind::Integer => "int",
            Kind::Float => "float",
            Kind::String => "str",
            Kind::Array(k, f, t) =>
                return format!("arr<{}>[{},{}]", &k.mangle_name(), f, t),
            Kind::Void => "void",
        }.to_string()
    }
}

impl Declaration {
    fn mangle_name(&self) -> String {
        let base = "user_".to_string() + &self.name;
        self.params.iter().fold(base,
            |x, y| x + "_" + &y.kind.mangle_name())
            + "+" + &self.return_type.mangle_name()
    }
}

impl Program {
    fn find_duplicate_names_vars_constants(&self) -> Outcome<()> {
        let mut names: HashSet<&String> = HashSet::new();

        for var in self.scope.vars.iter() {
            if names.contains(&var.name) {
                return Err(MilaErr::DuplicateGlobal(var.name.clone()));
            }
            names.insert(&var.name);
        }
        for constant in self.scope.constants.iter() {
            if names.contains(&constant.name) {
                return Err(MilaErr::DuplicateGlobal(constant.name.clone()));
            }
            names.insert(&constant.name);
        }
        Ok(())
    }

    fn find_invalid_functions(&self) -> Outcome<()> {
        let mut names: HashSet<&String> = HashSet::new();
        names.insert(&"main".to_string());

        for declaration in self.scope.declarations.iter() {
            if names.contains(&declaration.name) {
                return Err(MilaErr::DuplicateFunName(declaration.name));
            }
            names.insert(&declaration.name);
        }

        Ok(())
    }

    pub fn compile(&self) -> Outcome<()> {
        // llvm context
        let context = Context::create();
        let module = context.create_module(&self.name);
        let builder = context.create_builder();
        let llvm = LLVM {
            ctx: &context,
            mdl: module,
            bld: builder,
            address_space: AddressSpace::from(42),
        };
        // check vars/constants duplicates
        self.find_duplicate_names_vars_constants()?;
        self.find_invalid_functions()?;

        llvm.compile_step_2(self)?;

        // TODO
        llvm.mdl.verify();

        todo!()
    }
}

impl<'a> LLVM<'a> {
    fn compile_step_2(&self, prog: &Program) -> Outcome<()> {
        let llvm = self;
        let mut table = SymbolTable {
            symbols: vec![],
            functions: HashMap::new(),
        };

        let mut root_map = HashMap::new();
        for var in prog.scope.vars.iter() {
            let ptr = llvm.compile_global_variable(var)?;
            root_map.insert(
                var.name.clone(),
                VarSymbol {
                    kind: var.kind.clone(),
                    ptr,
                    is_const: false,
                },
            );
        }
        for constant in prog.scope.constants.iter() {
            let ptr = self.compile_global_constant(constant)?;
            root_map.insert(
                constant.name.clone(),
                VarSymbol {
                    kind: constant.val.to_type(),
                    ptr: ptr,
                    is_const: true,
                },
            );
        }
        table.symbols.push(root_map);

        for declaration in prog.scope.declarations.iter() {
            self.declare_function(declaration);
            table.functions.insert(
                declaration.name.clone(),
                FunSymbol { declaration: declaration.clone(), value: self.declare_function(declaration)? },
            );
        }
        self.declare_builtin_functions();

        for function in prog.scope.functions.iter() {
            self.compile_function(&table, function);
        }

        self.compile_main(&table, &prog.scope.main);

        Ok(())
    }

    fn find_var_in_table(
        table: &'a SymbolTable<'a>,
        name: &str,
        for_write: bool,
    ) -> Outcome<&'a VarSymbol<'a>> {
        for map in table.symbols.iter().rev() {
            match map.get(name) {
                Some(symbol) => {
                    return if symbol.is_const && for_write {
                        Err(MilaErr::CannotChangeConstantVariable(name.to_string()))
                    } else {
                        Ok(symbol)
                    }
                }
                None => {}
            }
        }
        Err(MilaErr::VarNotFound(name.to_string()))
    }

    fn kind_to_llvm(&self, kind: &Kind) -> Outcome<BasicTypeEnum<'a>> {
        Ok(match kind {
            Kind::Integer => self.ctx.i64_type().into(),

            Kind::Float => self.ctx.f64_type().into(),

            Kind::String => self.ctx.i8_type().ptr_type(self.address_space).into(),

            Kind::Array(t, from, to) => self
                .kind_to_llvm(t)?
                .array_type((to - from + 1) as u32)
                .into(),

            Kind::Void => panic!("Void type is not transferable to llvm"),
        })
    }

    fn compile_global_constant(&self, constant: &Constant) -> Outcome<PointerValue<'a>> {
        let space = self.mdl.add_global(
            self.kind_to_llvm(&constant.val.to_type())?,
            Some(self.address_space),
            &constant.name,
        );

        let value: BasicValueEnum = match constant.val.clone() {
            Value::IntValue(val) => self.ctx.i64_type().const_int(val, true).into(),
            Value::FloatValue(val) => self.ctx.f64_type().const_float(val).into(),
            Value::StringValue(val) => {
                let value = self.bld.build_global_string_ptr(&val, "const_string");
                value.as_basic_value_enum()
            }
        };

        space.set_initializer(&value);
        space.set_constant(true);

        Ok(space.as_pointer_value())
    }

    fn declare_function(&self, function: &Declaration) -> Outcome<FunctionValue<'a>> {
        let mut args = vec![];
        for arg in function.params {
            args.push(self.kind_to_llvm(&arg.kind)?.into());
        }
        let args = args;

        let fn_type = if Kind::Void == function.return_type {
            self.ctx.void_type().fn_type(args.as_slice(), false)
        } else {
            self.kind_to_llvm(&function.return_type)?
                .fn_type(args.as_slice(), false)
        };

        Ok(self.mdl.add_function(&function.name, fn_type, None))
    }

    fn declare_builtin_functions(&self) -> Outcome<()> {
        // Write
        {
            let args = vec![
                self.kind_to_llvm(&Kind::String)?.into(),
            ];
            let fun = self.ctx.void_type().fn_type(args.as_slice(), false);
            self.mdl.add_function("built_wrileln", fun, None);
        };
        // ReadLine
        {
            let args = vec![
                self.ctx.i64_type().ptr_type(self.address_space).into(),
            ];
            let fun = self.ctx.i64_type()
                .fn_type(args.as_slice(), false);
            self.mdl.add_function("built_readln", fun, None);
        };
        Ok(())
    }

    fn compile_global_variable(&self, var: &Variable) -> Outcome<PointerValue<'a>> {
        Ok(self
            .mdl
            .add_global(
                self.kind_to_llvm(&var.kind)?,
                Some(self.address_space),
                &var.name,
            )
            .as_pointer_value())
    }

    fn compile_statement(
        &self,
        statement: &Statement,
        symbols: &'a SymbolTable<'a>,
    ) -> Outcome<InstructionValue<'a>> {
        Ok(match statement {
            Statement::Block { statements } => todo!(),
            Statement::ExprWrapper(expr) => self
                .compile_expr(expr, symbols)?
                .as_instruction_value()
                .unwrap(),
            Statement::Assign { space, expr } => {
                ///////////////////////////////////////////////////////////////
                // TODO add cast float to int
                ///////////////////////////////////////////////////////////////

                let value = self.compile_expr(expr, symbols)?;
                let space = self.get_mem_space(space, symbols, true)?;
                self.bld.build_store(space.0, value)
            }
            Statement::For {
                var_name,
                from,
                to,
                is_to,
                scope,
            } => todo!(),
            Statement::While { cond, scope } => todo!(),
            Statement::If { cond, true_branch } => todo!(),
            Statement::IfElse {
                cond,
                true_branch,
                false_branch,
            } => todo!(),
            Statement::Exit => todo!(),
        })
    }

    fn cast_to_same(
        &self,
        lhs: BasicValueEnum<'a>,
        rhs: BasicValueEnum<'a>,
        name: &str,
    ) -> Outcome<ValuePair<'a>> {
        if lhs.is_int_value() && rhs.is_int_value() {
            return Ok(ValuePair::IntPair(
                lhs.into_int_value(),
                rhs.into_int_value(),
            ));
        };

        let lhs = if lhs.is_int_value() {
            self.bld
                .build_cast(
                    InstructionOpcode::SIToFP,
                    lhs.into_int_value(),
                    self.ctx.f64_type(),
                    name,
                )
                .into_float_value()
        } else if lhs.is_float_value() {
            lhs.into_float_value()
        } else {
            return Err(MilaErr::WrongCast);
        };
        let rhs = if rhs.is_int_value() {
            self.bld
                .build_cast(
                    InstructionOpcode::SIToFP,
                    rhs.into_int_value(),
                    self.ctx.f64_type(),
                    name,
                )
                .into_float_value()
        } else if rhs.is_float_value() {
            rhs.into_float_value()
        } else {
            return Err(MilaErr::WrongCast);
        };

        Ok(ValuePair::FloatPair(lhs, rhs))
    }

    fn compile_expr(
        &self,
        expr: &Expr,
        symbols: &'a SymbolTable<'a>,
    ) -> Outcome<BasicValueEnum<'a>> {
        Ok(match expr {
            Expr::Add(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "add")? {
                    ValuePair::IntPair(lhs, rhs) => {
                        self.bld.build_int_add(lhs, rhs, "add_int").into()
                    }
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_add(lhs, rhs, "add_float").into()
                    }
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "sub")? {
                    ValuePair::IntPair(lhs, rhs) => self.bld.build_int_sub(lhs, rhs, "int").into(),
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_sub(lhs, rhs, "float").into()
                    }
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "mul")? {
                    ValuePair::IntPair(lhs, rhs) => {
                        self.bld.build_int_mul(lhs, rhs, "mul_int").into()
                    }
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_mul(lhs, rhs, "mul_float").into()
                    }
                }
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "div")? {
                    ValuePair::IntPair(lhs, rhs) => {
                        self.bld.build_int_signed_div(lhs, rhs, "div_int").into()
                    }
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_div(lhs, rhs, "div_float").into()
                    }
                }
            }
            Expr::Mod(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "mod")? {
                    ValuePair::IntPair(lhs, rhs) => {
                        self.bld.build_int_signed_rem(lhs, rhs, "mod_int").into()
                    }
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_rem(lhs, rhs, "mod_float").into()
                    }
                }
            }
            Expr::Gt(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "gt")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "gt_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::UGT, lhs, rhs, "gt_float")
                        .into(),
                }
            }
            Expr::Ge(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "ge")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, "ge_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::UGE, lhs, rhs, "ge_float")
                        .into(),
                }
            }
            Expr::Lt(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "lt")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "lt_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::ULT, lhs, rhs, "lt_float")
                        .into(),
                }
            }
            Expr::Le(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "le")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "le_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::ULE, lhs, rhs, "le_float")
                        .into(),
                }
            }
            Expr::Eq(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "eq")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "eq_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::UEQ, lhs, rhs, "eq_float")
                        .into(),
                }
            }
            Expr::Ne(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;

                match self.cast_to_same(lhs, rhs, "ne")? {
                    ValuePair::IntPair(lhs, rhs) => self
                        .bld
                        .build_int_compare(IntPredicate::NE, lhs, rhs, "ne_int")
                        .into(),
                    ValuePair::FloatPair(lhs, rhs) => self
                        .bld
                        .build_float_compare(FloatPredicate::UNE, lhs, rhs, "ne_float")
                        .into(),
                }
            }
            Expr::And(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                self.bld
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                    .into()
            }
            Expr::Or(lhs, rhs) => {
                let lhs = self.compile_expr(lhs, symbols)?;
                let rhs = self.compile_expr(rhs, symbols)?;
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                self.bld
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "and")
                    .into()
            }
            Expr::Literal(value) => match value {
                Value::IntValue(val) => self.ctx.i64_type().const_int(*val, true).into(),
                Value::FloatValue(val) => self.ctx.f64_type().const_float(*val).into(),
                Value::StringValue(val) => self
                    .bld
                    .build_global_string_ptr(val, "string_literal")
                    .as_basic_value_enum(),
            },
            Expr::FunCall { name, args } => {
                let mut lowered = vec![];
                for expr in args.iter() {
                    lowered.push(self.compile_expr(expr, symbols)?.into());
                }
                let args = lowered;

                let fun = symbols.functions.get(name).ok_or_else(|| MilaErr::FunctionNotDefined(name.clone()))?;
                let mangled = fun.declaration.mangle_name();

                let fun = self.mdl.get_function(&mangled).unwrap();

                self.bld.build_direct_call(fun, args.as_slice(), "function_call");
                todo!()
            }
            Expr::BuiltIn { name, args } => todo!(),
            Expr::VarAccess(name) => {
                let symbol = LLVM::find_var_in_table(symbols, name, false)?;
                self.bld
                    .build_load(self.kind_to_llvm(&symbol.kind)?, symbol.ptr, name)
            }
            Expr::ArrayAccess(store, _) => {
                let arr_ptr = self.get_mem_space(expr, symbols, false)?.0;
                self.bld
                    .build_load(arr_ptr.get_type(), arr_ptr, "array read")
            }
            _ => panic!(""),
        })
    }

    fn get_mem_space(
        &self,
        expr: &Expr,
        symbols: &'a SymbolTable<'a>,
        is_write: bool,
    ) -> Outcome<(PointerValue<'a>, Kind)> {
        Ok(match expr {
            // Expr::FunCall { name, args } => todo!(),
            Expr::VarAccess(name) => {
                let symbol = LLVM::find_var_in_table(symbols, name, is_write)?;
                (symbol.ptr, symbol.kind.clone())
            }
            Expr::ArrayAccess(store, index) => {
                let index = self.compile_expr(index, symbols)?;
                let index = if index.is_int_value() {
                    index.into_int_value()
                } else {
                    return Err(MilaErr::CannotIndexWithNonInteger);
                };

                let (dest, kind) = self.get_mem_space(store, symbols, is_write)?;

                let (subkind, index) = match kind {
                    Kind::Array(subkind, from, _) => {
                        let offset = self.ctx.i64_type().const_int(from.abs() as u64, true);
                        let signed = if from < 0 {
                            offset
                        } else {
                            let zero = self.ctx.i64_type().const_zero();
                            self.bld.build_int_sub(zero, offset, "inverse index")
                        };
                        let result = self.bld.build_int_add(index, signed, "update index");
                        (subkind, result)
                    }
                    _ => return Err(MilaErr::CannotUseIndexingOnNonArrayType { code: 1 }),
                };

                let array = if dest.as_basic_value_enum().is_array_value() {
                    dest.as_basic_value_enum().into_array_value()
                } else {
                    return Err(MilaErr::CannotUseIndexingOnNonArrayType { code: 2 });
                };

                (
                    unsafe {
                        self.bld
                            .build_gep(array.get_type(), dest, &[index], "get, array, scary")
                    },
                    *subkind,
                )
            }
            _ => return Err(MilaErr::AssignNotSupported(expr.clone())),
        })
    }

    fn compile_function(
        &self,
        symbols: &'a SymbolTable<'a>,
        function: &Function,
    ) -> Outcome<()> {
        let fun_symbol = symbols
            .functions.get(&function.name)
            .ok_or_else(|| MilaErr::FunctionNotDefined(function.name.clone()))?;
        let declaration = &fun_symbol.declaration;

        let fun = self.mdl.get_function(&declaration.mangle_name()).unwrap();

        let mut vars = HashMap::new();
        for (i, arg) in function.vars.iter().enumerate() {
            let param = fun.get_nth_param(i.try_into().unwrap()).unwrap();
            param.set_name(&arg.name);
            vars.insert(arg.name.clone(), VarSymbol{
                kind: arg.kind.clone(), ptr: param.into_pointer_value(), is_const: false
            });
        }

        let return_alloc = self.bld.build_alloca(self.kind_to_llvm(&declaration.return_type)?, "return_var");
        vars.insert(declaration.name, VarSymbol{
            kind: declaration.return_type, ptr: return_alloc, is_const: false
        });

        symbols.symbols.push(vars);

        let bb = self.ctx.append_basic_block(fun, "fun_compilation");
        
        let _instruction = self.compile_statement(&function.statement, symbols)?;
        
        // fallback return
        let _return_val = if declaration.return_type == Kind::Void {
            self.bld.build_return(None)
        } else {

            let to_return = self.try_cast_into_kind(return_alloc.as_basic_value_enum(), declaration.return_type)?;
            self.bld.build_return(Some(&*to_return))
        };

        symbols.symbols.pop();

        return Ok(());
    }

    fn compile_main(
        &self,
        symbols: &'a SymbolTable<'a>,
        statement: &Statement,
    ) -> Outcome<()> {
        let name = "main".to_string();
        let mut vars = HashMap::new();
        let return_alloc = self.bld.build_alloca(self.ctx.i64_type(), "main_return_var");
        vars.insert(name, VarSymbol{
            kind: Kind::Integer, ptr: return_alloc, is_const: false
        });
        symbols.symbols.push(vars);

        let main_function = self.mdl.add_function(&name, self.ctx.i64_type().fn_type(&[], false), None);
        let entry = self.ctx.append_basic_block(main_function, "main_block");
        self.bld.position_at_end(entry);

        let _instruction = self.compile_statement(&statement, symbols)?;
        // fallback return
        self.bld.build_return(Some(&self.ctx.i64_type().const_int(42, false)));

        symbols.symbols.pop();

        return Ok(());
    }

    fn try_cast_into_kind(&self, value: BasicValueEnum<'a>, kind: Kind) -> Outcome<Box<dyn BasicValue>>{
        if self.kind_to_llvm(&kind)?.type_id() == value.get_type().type_id() {
            let value: Box<dyn BasicValue> = match kind {
                Kind::Integer => Box::new(value.into_int_value()),
                Kind::Float => Box::new(value.into_float_value()),
                Kind::String => Box::new(value.into_pointer_value()),
                Kind::Array(_, _, _) => Box::new(value.into_array_value()),
                _ => return Err(MilaErr::WrongCast)
            };
            return Ok(value);
        }
        ;todo!()
    }
}
