use std::collections::{HashMap, HashSet};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{
        AsValueRef, BasicValue, BasicValueEnum, FloatValue, GlobalValue, InstructionOpcode,
        InstructionValue, IntValue, PointerValue,
    },
    AddressSpace,
};

use crate::{
    ast::{Constant, Expr, Kind, Program, Statement, Value, Variable},
    base::Outcome,
    errors::MilaErr,
};

struct Symbol<'a> {
    kind: Kind,
    ptr: PointerValue<'a>,
    is_const: bool,
}

struct LLVM<'a> {
    ctx: &'a Context,
    mdl: Module<'a>,
    bld: Builder<'a>,
    address_space: AddressSpace,
}

type Symbols<'a> = Vec<HashMap<String, Symbol<'a>>>;

struct LocalCtx {
    // function name
    // return type
    // code block
}

enum ValuePair<'a> {
    IntPair(IntValue<'a>, IntValue<'a>),
    FloatPair(FloatValue<'a>, FloatValue<'a>),
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

        llvm.compile_step_2(self)?;

        // TODO
        llvm.mdl.verify();

        todo!()
    }
}

impl<'a> LLVM<'a> {
    fn compile_step_2(&self, prog: &Program) -> Outcome<()> {
        let llvm = self;
        let mut symbols: Symbols = vec![];

        let mut root_map = HashMap::new();
        for var in prog.scope.vars.iter() {
            let ptr = llvm.compile_global_variable(var)?;
            root_map.insert(
                var.name.clone(),
                Symbol {
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
                Symbol {
                    kind: constant.val.to_type(),
                    ptr: ptr,
                    is_const: true,
                },
            );
        }
        symbols.push(root_map);

        // check functions
        // mangle function names
        // precompile functions

        // generate main
        // generate functions

        Ok(())
    }

    fn find_in_table(
        symbols: &'a Symbols<'a>,
        name: &str,
        for_write: bool,
    ) -> Outcome<&'a Symbol<'a>> {
        for map in symbols.iter().rev() {
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

    fn to_llvm(&self, kind: &Kind) -> Outcome<BasicTypeEnum<'a>> {
        Ok(match kind {
            Kind::Integer => self.ctx.i64_type().into(),

            Kind::Float => self.ctx.f64_type().into(),

            Kind::String => self.ctx.i8_type().ptr_type(self.address_space).into(),

            Kind::Array(t, from, to) => self.to_llvm(t)?.array_type((to - from + 1) as u32).into(),

            Kind::Void => panic!("Void type is not transferable to llvm"),
        })
    }

    fn compile_global_constant(&self, constant: &Constant) -> Outcome<PointerValue<'a>> {
        let space = self.mdl.add_global(
            self.to_llvm(&constant.val.to_type())?,
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

    fn compile_global_variable(&self, var: &Variable) -> Outcome<PointerValue<'a>> {
        Ok(self
            .mdl
            .add_global(
                self.to_llvm(&var.kind)?,
                Some(self.address_space),
                &var.name,
            )
            .as_pointer_value())
    }

    fn compile_statement(
        &self,
        statement: &Statement,
        symbols: &'a Symbols<'a>,
    ) -> Outcome<InstructionValue<'a>> {
        Ok(match statement {
            Statement::Block { statements } => todo!(),
            Statement::ExprWrapper(expr) => self
                .compile_expr(expr, symbols)?
                .as_instruction_value()
                .unwrap(),
            Statement::Assign { space, expr } => {
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

    fn compile_expr(&self, expr: &Expr, symbols: &'a Symbols<'a>) -> Outcome<BasicValueEnum<'a>> {
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
            Expr::Sub(_, _) => todo!(),
            Expr::Mul(_, _) => todo!(),
            Expr::Div(_, _) => todo!(),
            Expr::Mod(_, _) => todo!(),
            Expr::Gt(_, _) => todo!(),
            Expr::Ge(_, _) => todo!(),
            Expr::Lt(_, _) => todo!(),
            Expr::Le(_, _) => todo!(),
            Expr::Eq(_, _) => todo!(),
            Expr::Ne(_, _) => todo!(),
            Expr::And(_, _) => todo!(),
            Expr::Or(_, _) => todo!(),
            Expr::Literal(value) => match value {
                Value::IntValue(val) => self.ctx.i64_type().const_int(*val, true).into(),
                Value::FloatValue(val) => self.ctx.f64_type().const_float(*val).into(),
                Value::StringValue(val) => self
                    .bld
                    .build_global_string_ptr(val, "string_literal")
                    .as_basic_value_enum(),
            },
            Expr::FunCall { name, args } => todo!(),
            Expr::BuiltIn { name, args } => todo!(),
            Expr::VarAccess(name) => {
                let symbol = LLVM::find_in_table(symbols, name, false)?;
                self.bld
                    .build_load(self.to_llvm(&symbol.kind)?, symbol.ptr, name)
            }
            Expr::ArrayAccess(store, _) => {
                let arr_ptr = self.get_mem_space(expr, symbols, false)?.0;
                self.bld
                    .build_load(arr_ptr.get_type(), arr_ptr, "array read")
            },
            _ => panic!("")
        })
    }

    fn get_mem_space(
        &self,
        expr: &Expr,
        symbols: &'a Symbols<'a>,
        is_write: bool,
    ) -> Outcome<(PointerValue<'a>, Kind)> {
        Ok(match expr {
            // Expr::FunCall { name, args } => todo!(),
            Expr::VarAccess(name) => {
                let symbol = LLVM::find_in_table(symbols, name, is_write)?;
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
                    },
                    _ => return Err(MilaErr::CannotUseIndexingOnNonArrayType { code: 1 })
                };

                let array = if dest.as_basic_value_enum().is_array_value() {
                    dest.as_basic_value_enum().into_array_value()
                } else {
                    return Err(MilaErr::CannotUseIndexingOnNonArrayType { code: 2 });
                };

                (unsafe {
                    self.bld.build_gep(array.get_type(), dest, &[index], "get, array, scary")
                }, *subkind)
            }
            _ => return Err(MilaErr::AssignNotSupported(expr.clone())),
        })
    }
}

fn idk() {
    let context = Context::create();
    let module = context.create_module("main_module");
    let builder = context.create_builder();
    let main_function = module.add_function("main", context.i32_type().fn_type(&[], false), None);
    let entry = context.append_basic_block(main_function, "entry");
    builder.position_at_end(entry);
    builder.build_return(Some(&context.i32_type().const_int(42, false)));
    println!("{}", module.print_to_string().to_string());
}
