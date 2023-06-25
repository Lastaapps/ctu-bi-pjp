use std::collections::{HashMap, HashSet};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum},
    values::{
        BasicValue, BasicValueEnum, FloatValue, FunctionValue, InstructionOpcode, IntValue,
        PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    ast::{Constant, Declaration, Expr, Function, Kind, Program, Statement, Value, Variable},
    base::Outcome,
    errors::MilaErr,
    tokens::BuiltInType,
};

#[derive(Debug, Clone)]
struct VarSymbol<'a> {
    kind: Kind,
    ptr: PointerValue<'a>,
    is_const: bool,
}

#[derive(Debug, Clone)]
struct FunSymbol<'a> {
    declaration: Declaration,
    value: FunctionValue<'a>,
}

struct LLVM<'a> {
    ctx: &'a Context,
    mdl: Module<'a>,
    bld: Builder<'a>,
    mila: MilaCtx<'a>,
}

struct MilaCtx<'a> {
    symbols: Vec<HashMap<String, VarSymbol<'a>>>,
    functions: HashMap<String, FunSymbol<'a>>,
    curr_function: Option<(String, Kind)>,
    break_continue_target: Option<(BasicBlock<'a>, BasicBlock<'a>)>,
}

const MAIN_NAME: &str = "main";

enum ValuePair<'a> {
    IntPair(IntValue<'a>, IntValue<'a>),
    FloatPair(FloatValue<'a>, FloatValue<'a>),
}

impl BuiltInType {
    fn fn_name(&self, kind: Option<&Kind>) -> String {
        let suffix = match kind {
            Some(kind) => {
                "_".to_string()
                    + match kind {
                        Kind::Integer => "integer",
                        Kind::Float => "float",
                        Kind::String => "string",
                        _ => panic!("Not supported type: {:?}", kind),
                    }
            }
            None => "".to_string(),
        };
        match self {
            BuiltInType::ReadLine => "built_in_read_line",
            BuiltInType::Write => "built_in_write",
            _ => panic!("Kind name not supported for {self:?}"),
        }
        .to_string()
            + &suffix
    }
}

impl Kind {
    fn mangle_name(&self) -> String {
        match self {
            Kind::Integer => "int",
            Kind::Float => "float",
            Kind::String => "string",
            Kind::Array(k, f, t) => return format!("arr<{}>[{},{}]", &k.mangle_name(), f, t),
            Kind::Void => "void",
        }
        .to_string()
    }
}

impl Declaration {
    fn mangle_name(&self) -> String {
        let base = "user_".to_string() + &self.name;
        self.params
            .iter()
            .fold(base, |x, y| x + "_" + &y.kind.mangle_name())
            + ":"
            + &self.return_type.mangle_name()
    }
}

const TAG: &str = "llvm";
fn log(msg: &str) {
    eprintln!("{}: {}", TAG, msg)
}

impl Program {
    fn find_duplicate_names_vars_constants(&self) -> Outcome<()> {
        log("Looking for duplicate var names");

        let mut names: HashSet<&String> = HashSet::new();

        for var in self.scope.vars.iter() {
            if names.contains(&var.name) {
                return Err(MilaErr::DuplicateGlobal(var.name.clone()));
            }
            names.insert(&var.name);
        }

        log("Looking for duplicate const names");

        for constant in self.scope.constants.iter() {
            if names.contains(&constant.name) {
                return Err(MilaErr::DuplicateGlobal(constant.name.clone()));
            }
            names.insert(&constant.name);
        }
        Ok(())
    }

    fn find_invalid_functions(&self) -> Outcome<()> {
        log("Looking for duplicate fun defs");

        let mut names: HashSet<&String> = HashSet::new();
        let main_name = MAIN_NAME.to_string();
        names.insert(&main_name);

        for declaration in self.scope.declarations.iter() {
            if declaration.1 == None {
                continue; // Will be checked bellow
            }

            if names.contains(&declaration.0.name) {
                return Err(MilaErr::DuplicateFunName(declaration.0.name.clone()));
            }
            names.insert(&declaration.0.name);

            // check for fun name and one of the params is the same
            let mut names: HashSet<&String> = HashSet::new();
            names.insert(&declaration.0.name);
            for param in declaration.0.params.iter() {
                if names.contains(&param.name) {
                    return Err(MilaErr::DuplicateFunAndParamName(param.name.clone()));
                }
                names.insert(&param.name);
            }
        }

        let mut defined = HashSet::new();
        for declaration in self.scope.declarations.iter() {
            if declaration.1 == None { continue; }
            defined.insert(declaration.0.mangle_name());
        }
        for declaration in self.scope.declarations.iter() {
            if let Some(_) = declaration.1 { continue; }
            if !defined.contains(&declaration.0.mangle_name()) {
                return Err(MilaErr::DeclaredNotDefined(declaration.0.name.clone()));
            }
        }

        Ok(())
    }

    pub fn compile(&self) -> Outcome<String> {
        log("Preparing llvm compilation");

        let mila = MilaCtx {
            symbols: vec![],
            functions: HashMap::new(),
            curr_function: None,
            break_continue_target: None,
        };

        // llvm context
        let context = Context::create();
        let module = context.create_module(&self.name);
        let builder = context.create_builder();
        let mut llvm = LLVM {
            ctx: &context,
            mdl: module,
            bld: builder,
            mila: mila,
        };
        log("Created compilation context");

        // check vars/constants duplicates
        self.find_duplicate_names_vars_constants()?;
        self.find_invalid_functions()?;

        llvm.compile_step_2(self)?;

        log("Starting verification");
        llvm.mdl.print_to_stderr();
        match llvm.mdl.verify() {
            Ok(_) => {}
            Err(e) => eprintln!("\n\n{}\n\n", e.to_string().replace("\\n", "\n")),
        };

        Ok(llvm.mdl.print_to_string().to_string())
    }
}

impl<'a> LLVM<'a> {
    fn compile_step_2(&mut self, prog: &Program) -> Outcome<()> {
        let mut root_map = HashMap::new();
        log("Creating global variables");
        for var in prog.scope.vars.iter() {
            let ptr = self.compile_global_variable(var)?;
            root_map.insert(
                var.name.clone(),
                VarSymbol {
                    kind: var.kind.clone(),
                    ptr,
                    is_const: false,
                },
            );
        }
        log("Creating global constants");
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
        self.mila.symbols.push(root_map);

        log("Declaring built in functions");
        self.declare_builtin_functions()?;

        for (declaration, func) in prog.scope.declarations.iter() {
            if !self.mila.functions.contains_key(&declaration.name) {
                log("Declaring function");
                let value = self.declare_function(declaration)?;
                self.mila.functions.insert(
                    declaration.name.clone(),
                    FunSymbol {
                        declaration: declaration.clone(),
                        value: value,
                    },
                );
            }
            if let Some(func) = func {
                log("Compiling function");
                self.compile_function(func)?;
            }
        }

        self.compile_main(&prog.scope.main)?;

        Ok(())
    }

    fn find_var_in_table(&mut self, name: &str, for_write: bool) -> Outcome<VarSymbol<'a>> {
        log("Finding var in a table");
        for map in self.mila.symbols.iter().rev() {
            match map.get(name) {
                Some(symbol) => {
                    return if symbol.is_const && for_write {
                        Err(MilaErr::CannotChangeConstantVariable(name.to_string()))
                    } else {
                        Ok(symbol.clone())
                    }
                }
                None => {}
            }
        }
        Err(MilaErr::VarNotFound(name.to_string()))
    }

    fn kind_to_llvm(&self, kind: &Kind) -> Outcome<BasicTypeEnum<'a>> {
        log("Kind to llvm");
        Ok(match kind {
            Kind::Integer => self.ctx.i64_type().into(),

            Kind::Float => self.ctx.f64_type().into(),

            Kind::String => self.ctx.i8_type().ptr_type(AddressSpace::default()).into(),

            Kind::Array(t, from, to) => self
                .kind_to_llvm(t)?
                .array_type((to - from + 1) as u32)
                .into(),

            Kind::Void => panic!("Void type is not transferable to llvm"),
        })
    }

    fn llvm_to_kind(&self, value: &BasicTypeEnum<'a>) -> Kind {
        if value.is_int_type() {
            Kind::Integer
        } else if value.is_float_type() {
            Kind::Float
        } else if value.is_array_type() {
            let element = self.llvm_to_kind(&value.into_array_type().get_element_type());
            Kind::Array(Box::new(element), 0, 0)
        } else if value.is_pointer_type() {
            Kind::String
        } else {
            panic!("Unexpected llvm type")
        }
    }

    fn compile_global_constant(&self, constant: &Constant) -> Outcome<PointerValue<'a>> {
        let space = self.mdl.add_global(
            self.kind_to_llvm(&constant.val.to_type())?,
            Some(AddressSpace::default()),
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

    fn declare_function(&self, declaration: &Declaration) -> Outcome<FunctionValue<'a>> {
        let mut args = vec![];
        for arg in declaration.params.iter() {
            args.push(self.kind_to_llvm(&arg.kind)?.into());
        }
        let args = args;

        let fn_type = if Kind::Void == declaration.return_type {
            self.ctx.void_type().fn_type(args.as_slice(), false)
        } else {
            self.kind_to_llvm(&declaration.return_type)?
                .fn_type(args.as_slice(), false)
        };

        Ok(self
            .mdl
            .add_function(&declaration.mangle_name(), fn_type, None))
    }

    fn declare_builtin_functions(&self) -> Outcome<()> {
        // Write
        {
            // int
            let args = vec![self.kind_to_llvm(&Kind::Integer)?.into()];
            let fun = self.ctx.i64_type().fn_type(args.as_slice(), false);
            self.mdl.add_function(
                &BuiltInType::Write.fn_name(Some(&Kind::Integer)),
                fun,
                Some(Linkage::External),
            );

            // float
            let args = vec![self.kind_to_llvm(&Kind::Float)?.into()];
            let fun = self.ctx.f64_type().fn_type(args.as_slice(), false);
            self.mdl.add_function(
                &BuiltInType::Write.fn_name(Some(&Kind::Float)),
                fun,
                Some(Linkage::External),
            );

            // string
            let args = vec![self.kind_to_llvm(&Kind::String)?.into()];
            let fun = self.ctx.i64_type().fn_type(args.as_slice(), false);
            self.mdl.add_function(
                &BuiltInType::Write.fn_name(Some(&Kind::String)),
                fun,
                Some(Linkage::External),
            );
        };
        // ReadLine
        {
            // int
            let args = vec![self.ctx.i64_type().ptr_type(AddressSpace::default()).into()];
            let fun = self.ctx.i64_type().fn_type(args.as_slice(), false);
            self.mdl.add_function(
                &BuiltInType::ReadLine.fn_name(Some(&Kind::Integer)),
                fun,
                Some(Linkage::External),
            );

            // float
            let args = vec![self.ctx.f64_type().ptr_type(AddressSpace::default()).into()];
            let fun = self.ctx.f64_type().fn_type(args.as_slice(), false);
            self.mdl.add_function(
                &BuiltInType::ReadLine.fn_name(Some(&Kind::Float)),
                fun,
                Some(Linkage::External),
            );
        };
        Ok(())
    }

    fn compile_global_variable(&self, var: &Variable) -> Outcome<PointerValue<'a>> {
        let space = self.mdl.add_global(
            self.kind_to_llvm(&var.kind)?,
            Some(AddressSpace::default()),
            &var.name,
        );

        // haven't found an elegant way how to just take up the space
        // without this being called inside the main code block
        let value: BasicValueEnum = match &var.kind {
            Kind::Integer => self.ctx.i64_type().const_int(0, true).into(),
            Kind::Float => self.ctx.f64_type().const_float(0.0).into(),
            Kind::String => {
                let value = self.bld.build_global_string_ptr(&"", "const_string");
                value.as_basic_value_enum()
            }
            Kind::Array(element_kind, from, to) => self
                .kind_to_llvm(&element_kind)?
                .array_type((to - from + 1) as u32)
                .const_zero()
                .as_basic_value_enum(),
            kind => panic!("No other type is expected, not even {:?}", kind),
        };

        space.set_initializer(&value);

        Ok(space.as_pointer_value())
    }

    fn compile_statement(&mut self, statement: &Statement) -> Outcome<()> {
        match statement {
            Statement::Block { statements } => {
                log("Compiling block");
                for statement in statements.iter() {
                    self.compile_statement(statement)?;
                }
            }
            Statement::ExprWrapper(expr) => {
                log("Compile expr wrapper");
                self.compile_expr(expr)?;
            }
            Statement::Assign { space, expr } => {
                log("Compile assign");
                let value = self.compile_expr(expr)?;
                let (space, kind) = self.get_mem_space(space, true)?;
                let casted = self.try_cast_into_kind(value, &kind)?;
                self.bld.build_store(space, casted.as_basic_value_enum());
            }
            Statement::For {
                var_name,
                from,
                to,
                is_to,
                scope,
            } => {
                log("Compile for");
                let int_type = self.ctx.i64_type();
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let cond_cb = self.ctx.append_basic_block(parent_func, "for_cond");
                let body_cb = self.ctx.append_basic_block(parent_func, "for_body");
                let inc_cb = self.ctx.append_basic_block(parent_func, "for_inc");
                let follow_cb = self.ctx.append_basic_block(parent_func, "for_follow");

                let (space, kind) =
                    self.get_mem_space(&Expr::VarAccess(var_name.to_string()), true)?;
                if kind != Kind::Integer {
                    return Err(MilaErr::ForIntOnly);
                }

                let from = self.compile_expr(from)?;
                if !from.is_int_value() {
                    return Err(MilaErr::ForIntOnly);
                }
                self.bld.build_store(space, from);

                self.bld.build_unconditional_branch(cond_cb);
                self.bld.position_at_end(cond_cb);

                let to = self.compile_expr(to)?;
                let to = if !to.is_int_value() {
                    return Err(MilaErr::ForIntOnly);
                } else {
                    to.into_int_value()
                };
                let to = match is_to {
                    true => self.bld.build_int_add(to, to.get_type().const_int(1, true), "modify_to"),
                    false => self.bld.build_int_sub(to, to.get_type().const_int(1, true), "modify_to"),
                };

                let current = self
                    .bld
                    .build_load(self.ctx.i64_type(), space, "for_load")
                    .into_int_value();

                log("For: cond");
                let cond = self
                    .bld
                    .build_int_compare(IntPredicate::NE, current, to, "for_compare");
                self.bld.build_conditional_branch(cond, body_cb, follow_cb);

                log("For: body");
                let old_target = self.mila.break_continue_target;
                self.mila.break_continue_target = Some((follow_cb.clone(), inc_cb.clone()));

                self.bld.position_at_end(body_cb);
                self.compile_statement(&scope)?;
                self.bld.build_unconditional_branch(inc_cb);

                log("For - inc/dec");
                // +- 1
                self.bld.position_at_end(inc_cb);
                let loaded = self
                    .bld
                    .build_load(int_type, space, "for_int_load")
                    .into_int_value();
                let one = int_type.const_int(1, true);
                let modify = if *is_to {
                    self.bld.build_int_add(loaded, one, "for_inc")
                } else {
                    self.bld.build_int_sub(loaded, one, "for_dec")
                };
                self.bld.build_store(space, modify);
                self.bld.build_unconditional_branch(cond_cb);

                self.bld.position_at_end(follow_cb);
                self.mila.break_continue_target = old_target;
            }
            Statement::While { cond, scope } => {
                log("Compiling while");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let cond_cb = self.ctx.append_basic_block(parent_func, "while_cond");
                let body_cb = self.ctx.append_basic_block(parent_func, "while_body");
                let follow_cb = self.ctx.append_basic_block(parent_func, "while_follow");

                self.bld.build_unconditional_branch(cond_cb);
                self.bld.position_at_end(cond_cb);

                log("While: cond");
                let cond = self.compile_expr(cond)?;
                let cond = self
                    .try_cast_into_kind(cond, &Kind::Integer)?
                    .as_basic_value_enum()
                    .into_int_value();
                let cond = self.to_bool_int(&cond);
                self.bld.build_conditional_branch(cond, body_cb, follow_cb);

                log("While: body");
                let old_target = self.mila.break_continue_target;
                self.mila.break_continue_target = Some((follow_cb.clone(), cond_cb.clone()));

                self.bld.position_at_end(body_cb);
                self.compile_statement(&scope)?;
                self.bld.build_unconditional_branch(cond_cb);

                self.bld.position_at_end(follow_cb);
                self.mila.break_continue_target = old_target;
            }
            Statement::If { cond, true_branch } => {
                log("Compiling if");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let if_true_cb = self.ctx.append_basic_block(parent_func, "if_true");
                let follow_cb = self.ctx.append_basic_block(parent_func, "if_follow");

                log("if: cond");
                let cond = self.compile_expr(cond)?;
                let cond = self
                    .try_cast_into_kind(cond, &Kind::Integer)?
                    .as_basic_value_enum()
                    .into_int_value();

                let cond = self.to_bool_int(&cond);
                self.bld
                    .build_conditional_branch(cond, if_true_cb, follow_cb);

                log("if: true");
                self.bld.position_at_end(if_true_cb);
                self.compile_statement(&true_branch)?;
                self.bld.build_unconditional_branch(follow_cb);

                self.bld.position_at_end(follow_cb);
            }
            Statement::IfElse {
                cond,
                true_branch,
                false_branch,
            } => {
                log("Compiling if-else");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let if_true_cb = self.ctx.append_basic_block(parent_func, "if_true");
                let if_false_cb = self.ctx.append_basic_block(parent_func, "if_false");
                let follow_cb = self.ctx.append_basic_block(parent_func, "if_follow");

                log("if-else: cond");
                let cond = self.compile_expr(cond)?;
                let cond = if cond.is_int_value() {
                    cond.into_int_value()
                } else {
                    self.try_cast_into_kind(cond, &Kind::Integer)?
                        .as_basic_value_enum()
                        .into_int_value()
                };
                let cond = self.to_bool_int(&cond);

                self.bld
                    .build_conditional_branch(cond, if_true_cb, if_false_cb);

                log("if-else: true branch");
                self.bld.position_at_end(if_true_cb);
                self.compile_statement(&true_branch)?;
                self.bld.build_unconditional_branch(follow_cb);

                log("if-else: false branch");
                self.bld.position_at_end(if_false_cb);
                self.compile_statement(&false_branch)?;
                self.bld.build_unconditional_branch(follow_cb);

                self.bld.position_at_end(follow_cb);
            }
            Statement::Exit => {
                log("Compiling exit");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let follow_cb = self.ctx.append_basic_block(parent_func, "exit_follow");

                let (name, kind) = self.mila.curr_function.as_ref().unwrap().clone();
                let res = if kind == Kind::Void {
                    self.bld.build_return(None);
                } else {
                    let (space, _) = self.get_mem_space(&Expr::VarAccess(name.clone()), false)?;
                    let value = self
                        .bld
                        .build_load(self.kind_to_llvm(&kind)?, space, "exit_load");
                    self.bld
                        .build_return(Some(&*self.try_cast_into_kind(value, &kind)?));
                };
                self.bld.position_at_end(follow_cb);
                res
            }
            Statement::Break => {
                log("Compiling break");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let follow_cb = self.ctx.append_basic_block(parent_func, "exit_follow");

                let target = match self.mila.break_continue_target {
                    Some((target, _)) => target,
                    None => return Err(MilaErr::NoBreakContinueContext),
                };
                self.bld.build_unconditional_branch(target.clone());
                self.bld.position_at_end(follow_cb);
            }
            Statement::Continue => {
                log("Compiling continue");
                let prev_cb = self.bld.get_insert_block().unwrap();
                let parent_func = prev_cb.get_parent().unwrap();
                let follow_cb = self.ctx.append_basic_block(parent_func, "exit_follow");

                let target = match self.mila.break_continue_target {
                    Some((_, target)) => target,
                    None => return Err(MilaErr::NoBreakContinueContext),
                };
                self.bld.build_unconditional_branch(target.clone());
                self.bld.position_at_end(follow_cb);
            }
        };
        Ok(())
    }

    fn cast_to_same(
        &mut self,
        lhs: BasicValueEnum<'a>,
        rhs: BasicValueEnum<'a>,
        name: &str,
    ) -> Outcome<ValuePair<'a>> {
        log("Casting to same");

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

    fn compile_expr(&mut self, expr: &Expr) -> Outcome<BasicValueEnum<'a>> {
        Ok(match expr {
            Expr::Add(lhs, rhs) => {
                log("Add");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Sub");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                match self.cast_to_same(lhs, rhs, "sub")? {
                    ValuePair::IntPair(lhs, rhs) => self.bld.build_int_sub(lhs, rhs, "int").into(),
                    ValuePair::FloatPair(lhs, rhs) => {
                        self.bld.build_float_sub(lhs, rhs, "float").into()
                    }
                }
            }
            Expr::Mul(lhs, rhs) => {
                log("Mul");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Div");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Mod");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Gt");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Ge");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Lt");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Le");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Eq");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("Ne");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

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
                log("And");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                let lhs = self.to_bool_any(&lhs);
                let rhs = self.to_bool_any(&rhs);
                self.bld.build_and(lhs, rhs, "and").into()
            }
            Expr::Or(lhs, rhs) => {
                log("Or");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                let lhs = self.to_bool_any(&lhs);
                let rhs = self.to_bool_any(&rhs);
                self.bld.build_or(lhs, rhs, "or").into()
            }
            Expr::Xor(lhs, rhs) => {
                log("Xor");
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;
                if !lhs.is_int_value() || !rhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                let lhs = self.to_bool_any(&lhs);
                let rhs = self.to_bool_any(&rhs);
                self.bld.build_xor(lhs, rhs, "xor").into()
            }
            Expr::Not(lhs) => {
                log("Not");
                let lhs = self.compile_expr(lhs)?;
                if !lhs.is_int_value() {
                    return Err(MilaErr::LogicOnIntOnly);
                }
                let lhs = self.to_bool_any(&lhs);
                self.bld.build_not(lhs, "not").into()
            }
            Expr::CastToInt(lhs) => {
                log("Cast to int");
                let lhs = self.compile_expr(lhs)?;
                self.try_cast_into_kind(lhs, &Kind::Integer)?
                    .as_basic_value_enum()
            }
            Expr::CastToFloat(lhs) => {
                log("Cast to float");
                let lhs = self.compile_expr(lhs)?;
                self.try_cast_into_kind(lhs, &Kind::Float)?
                    .as_basic_value_enum()
            }
            Expr::Literal(value) => {
                log("Literal");
                match value {
                    Value::IntValue(val) => self.ctx.i64_type().const_int(*val, true).into(),
                    Value::FloatValue(val) => self.ctx.f64_type().const_float(*val).into(),
                    Value::StringValue(val) => self
                        .bld
                        .build_global_string_ptr(val, "string_literal")
                        .as_basic_value_enum(),
                }
            }
            Expr::FunCall { name, args } => {
                log("Fun call");
                let mut lowered = vec![];
                for expr in args.iter() {
                    lowered.push(self.compile_expr(expr)?.into());
                }
                let args = lowered;

                let fun = self
                    .mila
                    .functions
                    .get(name)
                    .ok_or_else(|| MilaErr::FunctionNotDeclared(name.clone()))?;
                let declaration = &fun.declaration;
                let mangled = declaration.mangle_name();

                if fun.declaration.params.len() != args.len() {
                    return Err(MilaErr::FunctionWrongArgCount { 
                        name: declaration.name.clone(),
                        exp: declaration.params.len(),
                        act: args.len() }
                    )
                }

                let fun_value = self.mdl.get_function(&mangled).unwrap();

                let call_site =
                    self.bld
                        .build_direct_call(fun_value, args.as_slice(), "function_call");
                if fun.declaration.return_type != Kind::Void {
                    call_site.try_as_basic_value().left().unwrap()
                } else {
                    // Would require some kind of trickery
                    // Perfect solution would be if the procedures
                    // were statements and ExprWrapper has some kind
                    // of detector for it. But this is nice enough
                    self.ctx.i64_type().const_int(42, true).into()
                }
            }
            Expr::BuiltIn {
                kind: built_in,
                args,
            } => {
                log("Built in");
                fn assert_size<T>(
                    built_in: &BuiltInType,
                    args: &Vec<T>,
                    size: usize,
                ) -> Outcome<()> {
                    if args.len() == size {
                        Ok(())
                    } else {
                        Err(MilaErr::BuiltInWrongArgCount(built_in.clone(), args.len()))
                    }
                }

                let int_type = self.ctx.i64_type();
                let float_type = self.ctx.f64_type();
                let new_line_args = vec![Expr::Literal(Value::StringValue("\n".to_string()))];

                match built_in {
                    BuiltInType::Dec => {
                        assert_size(built_in, args, 1)?;

                        let (space, kind) = self.get_mem_space(&args[0], true)?;
                        let loaded = self.bld.build_load(int_type, space, "for_int_load");
                        let res: BasicValueEnum = match kind {
                            Kind::Integer => {
                                let one = int_type.const_int(1, true);
                                self.bld
                                    .build_int_sub(loaded.into_int_value(), one, "inc_int")
                                    .into()
                            }
                            Kind::Float => {
                                let one = float_type.const_float(1.0);
                                self.bld
                                    .build_float_sub(loaded.into_float_value(), one, "inc_float")
                                    .into()
                            }
                            _ => {
                                return Err(MilaErr::AssignToDifferentType {
                                    exp: Kind::Integer,
                                    act: kind,
                                })
                            }
                        };
                        self.bld.build_store(space, res);
                        self.bld.build_load(int_type, space, "inc_final")
                    }
                    BuiltInType::Inc => {
                        assert_size(built_in, args, 1)?;

                        let (space, kind) = self.get_mem_space(&args[0], true)?;
                        let loaded = self.bld.build_load(int_type, space, "for_int_load");
                        let res: BasicValueEnum = match kind {
                            Kind::Integer => {
                                let one = int_type.const_int(1, true);
                                self.bld
                                    .build_int_add(loaded.into_int_value(), one, "inc_int")
                                    .into()
                            }
                            Kind::Float => {
                                let one = float_type.const_float(1.0);
                                self.bld
                                    .build_float_add(loaded.into_float_value(), one, "inc_float")
                                    .into()
                            }
                            _ => {
                                return Err(MilaErr::AssignToDifferentType {
                                    exp: Kind::Integer,
                                    act: kind,
                                })
                            }
                        };
                        self.bld.build_store(space, res);
                        self.bld.build_load(int_type, space, "inc_final")
                    }
                    BuiltInType::Write => {
                        assert_size(built_in, args, 1)?;

                        let space = self.compile_expr(&args[0])?;
                        let kind = self.llvm_to_kind(&space.get_type());

                        let func = self
                            .mdl
                            .get_function(&built_in.fn_name(Some(&kind)))
                            .unwrap();
                        let args = [self.compile_expr(&args[0])?.into()];

                        match kind {
                            Kind::Integer | Kind::Float => {
                                let args = [space.into()];
                                self.bld
                                    .build_indirect_call(
                                        func.get_type(),
                                        func.as_global_value().as_pointer_value(),
                                        &args,
                                        "write_call",
                                    )
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                            },
                            Kind::String => {
                                self.bld
                                    .build_indirect_call(
                                        func.get_type(),
                                        func.as_global_value().as_pointer_value(),
                                        &args,
                                        "write_call",
                                    )
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                            },
                            kind => panic!("Not supported type {:?}", kind),
                        }
                    }
                    BuiltInType::WriteLine => {
                        let res = self.compile_expr(&Expr::BuiltIn {
                            kind: BuiltInType::Write,
                            args: args.clone(),
                        })?;
                        self.compile_expr(&Expr::BuiltIn {
                            kind: BuiltInType::Write,
                            args: new_line_args,
                        })?;
                        res
                    }
                    BuiltInType::ReadLine => {
                        assert_size(built_in, args, 1)?;
                        let (space, kind) = self.get_mem_space(&args[0], true)?;
                        let func = self
                            .mdl
                            .get_function(&built_in.fn_name(Some(&kind)))
                            .unwrap();
                        let args = [space.into()];
                        self.bld
                            .build_indirect_call(
                                func.get_type(),
                                func.as_global_value().as_pointer_value(),
                                &args,
                                "read_call",
                            )
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                }
            }
            Expr::VarAccess(name) => {
                log("Var access");
                let symbol = self.find_var_in_table(name, false)?;
                self.bld
                    .build_load(self.kind_to_llvm(&symbol.kind)?, symbol.ptr, name)
            }
            Expr::ArrayAccess(_store, _) => {
                log("Array access");
                let (arr_ptr, kind) = self.get_mem_space(expr, false)?;
                self.bld
                    .build_load(self.kind_to_llvm(&kind)?, arr_ptr, "array_read")
            }
        })
    }

    fn get_mem_space(&mut self, expr: &Expr, is_write: bool) -> Outcome<(PointerValue<'a>, Kind)> {
        log("Mem space");
        Ok(match expr {
            // Expr::FunCall { name, args } => todo!(),
            Expr::VarAccess(name) => {
                let symbol = self.find_var_in_table(name, is_write)?;
                (symbol.ptr, symbol.kind.clone())
            }
            Expr::ArrayAccess(store, index) => {
                let index = self.compile_expr(index)?;
                let index = if index.is_int_value() {
                    index.into_int_value()
                } else {
                    return Err(MilaErr::CannotIndexWithNonInteger);
                };

                let (dest, kind) = self.get_mem_space(store, is_write)?;

                let (sub_kind, index) = match kind.clone() {
                    Kind::Array(sub_kind, from, _) => {
                        let offset = self.ctx.i64_type().const_int(from.abs() as u64, true);
                        let signed = if from < 0 {
                            offset
                        } else {
                            let zero = self.ctx.i64_type().const_zero();
                            self.bld.build_int_sub(zero, offset, "inverse_index")
                        };
                        let result = self.bld.build_int_add(index, signed, "update_index");
                        (*sub_kind, result)
                    }
                    _ => return Err(MilaErr::CannotUseIndexingOnNonArrayType { code: 1 }),
                };

                (
                    unsafe {
                        self.bld.build_gep(
                            self.kind_to_llvm(&kind)?,
                            dest,
                            &[index.get_type().const_zero(), index],
                            "array_element",
                        )
                    },
                    sub_kind,
                )
            }
            _ => return Err(MilaErr::AssignNotSupported(expr.clone())),
        })
    }

    fn compile_function(&mut self, function: &Function) -> Outcome<()> {
        log("Compiling function");

        let fun_symbol = self
            .mila
            .functions
            .get(&function.name)
            .ok_or_else(|| MilaErr::FunctionNotDeclared(function.name.clone()))?
            .clone();
        let declaration = &fun_symbol.declaration;

        let fun = self.mdl.get_function(&declaration.mangle_name()).unwrap();

        let bb = self.ctx.append_basic_block(fun, "fun_compilation");
        self.bld.position_at_end(bb);

        let mut vars = HashMap::new();

        log("Processing params");
        for (i, arg) in declaration.params.iter().enumerate() {
            let param = fun.get_nth_param(i.try_into().unwrap()).unwrap();
            param.set_name(&arg.name);

            let var = self
                .bld
                .build_alloca(self.kind_to_llvm(&arg.kind)?, &arg.name);
            self.bld.build_store(var, param);

            vars.insert(
                arg.name.clone(),
                VarSymbol {
                    kind: arg.kind.clone(),
                    ptr: var,
                    is_const: false,
                },
            );
        }

        log("Processing local vars");
        for arg in function.local_vars.iter() {
            let var = self
                .bld
                .build_alloca(self.kind_to_llvm(&arg.kind)?, &arg.name);
            vars.insert(
                arg.name.clone(),
                VarSymbol {
                    kind: arg.kind.clone(),
                    ptr: var,
                    is_const: false,
                },
            );
        }

        log("Building return");
        let return_alloc = if declaration.return_type != Kind::Void {
            let return_alloc = self
                .bld
                .build_alloca(self.kind_to_llvm(&declaration.return_type)?, "return_var");
            vars.insert(
                declaration.name.clone(),
                VarSymbol {
                    kind: declaration.return_type.clone(),
                    ptr: return_alloc,
                    is_const: false,
                },
            );
            Some(return_alloc)
        } else {
            None
        };

        self.mila.symbols.push(vars);
        self.mila.curr_function = Some((declaration.name.clone(), declaration.return_type.clone()));
        eprintln!("{:?}", self.mila.symbols);

        log("Compiling body");
        let _instruction = self.compile_statement(&function.statement)?;

        log("Compiling default return");
        // fallback return
        let _return_val = if declaration.return_type == Kind::Void {
            self.bld.build_return(None)
        } else {
            let loaded = self.bld.build_load(
                self.kind_to_llvm(&declaration.return_type)?,
                return_alloc.unwrap(),
                "fun_return_default",
            );
            let to_return = self.try_cast_into_kind(loaded, &declaration.return_type)?;
            self.bld.build_return(Some(&*to_return))
        };

        self.mila.symbols.pop();
        self.mila.curr_function = None;

        return Ok(());
    }

    fn compile_main(&mut self, statement: &Statement) -> Outcome<()> {
        log("Compiling main");

        let name = MAIN_NAME.to_string();

        log("Adding basic block");
        let main_function =
            self.mdl
                .add_function(&name, self.ctx.i64_type().fn_type(&[], false), None);
        let entry = self.ctx.append_basic_block(main_function, "main_block");
        self.bld.position_at_end(entry);

        log("Alloc return slot");
        let mut vars = HashMap::new();
        let return_alloc = self
            .bld
            .build_alloca(self.ctx.i64_type(), "main_return_var");

        vars.insert(
            name.clone(),
            VarSymbol {
                kind: Kind::Integer,
                ptr: return_alloc,
                is_const: false,
            },
        );
        self.mila.symbols.push(vars);
        self.mila.curr_function = Some((name.clone(), Kind::Integer));
        eprintln!("{:?}", self.mila.symbols);

        log("Compiling body");
        let _instruction = self.compile_statement(&statement)?;

        log("Compiling default return");
        self.bld
            .build_return(Some(&self.ctx.i64_type().const_int(42, false)));

        self.mila.symbols.pop();
        self.mila.curr_function = None;

        return Ok(());
    }

    fn try_cast_into_kind(
        &self,
        value: BasicValueEnum<'a>,
        kind: &Kind,
    ) -> Outcome<Box<dyn BasicValue<'a> + 'a>> {
        log("Try cast into");

        let val_kind = self.llvm_to_kind(&value.get_type());

        if kind == &val_kind {
            let value: Box<dyn BasicValue> = match kind {
                Kind::Integer => Box::new(value.into_int_value()),
                Kind::Float => Box::new(value.into_float_value()),
                Kind::String => Box::new(value.into_pointer_value()),
                Kind::Array(_, _, _) => Box::new(value.into_array_value()),
                _ => return Err(MilaErr::WrongCast),
            };
            return Ok(value);
        }
        if value.is_int_value() && kind == &Kind::Float {
            return Ok(Box::new(
                self.bld
                    .build_cast(
                        InstructionOpcode::SIToFP,
                        value.into_int_value(),
                        self.ctx.f64_type(),
                        "try_cast_int_to_float",
                    )
                    .into_float_value(),
            ));
        };
        if value.is_float_value() && kind == &Kind::Integer {
            return Ok(Box::new(
                self.bld
                    .build_cast(
                        InstructionOpcode::FPToSI,
                        value.into_float_value(),
                        self.ctx.i64_type(),
                        "try_cast_float_to_int",
                    )
                    .into_int_value(),
            ));
        };
        Err(MilaErr::AssignToDifferentType {
            exp: kind.clone(),
            act: val_kind,
        })
    }

    fn to_bool_any(&mut self, value: &BasicValueEnum<'a>) -> IntValue<'a> {
        log("To bool (any)");
        if !value.is_int_value() {
            panic!("Cast the value to int first");
        }
        let value = value.into_int_value();
        let zero = value.get_type().const_zero();
        self.bld.build_int_compare(
            IntPredicate::NE,
            value,
            zero,
            "to_bool_any",
        )
    }

    fn to_bool_int(&mut self, value: &IntValue<'a>) -> IntValue<'a> {
        log("To bool (int)");

        let zero = value.get_type().const_zero();
        self.bld
            .build_int_compare(IntPredicate::NE, value.clone(), zero, "to_bool_int")
    }
}
