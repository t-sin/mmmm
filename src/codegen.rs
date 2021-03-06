use crate::parse::{Block, Declare, Exp, InvokeFn, Statement, AST};
use crate::tokenize::{Keyword, Operator, Special};

fn generate_invoke_fn(invoke: &InvokeFn) -> String {
    let argstr = invoke
        .1
        .iter()
        .map(|e| generate_exp(e, 0))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({})", (invoke.0).0, argstr)
}

fn generate_if_body(list: &[Exp], nest: u64) -> String {
    if list.len() == 1 {
        format!("{}", generate_exp(&list[0], nest))
    } else {
        let mut s = String::new();
        s.push_str("\n");

        for exp in list.iter() {
            for _ in 0..nest + 1 {
                s.push_str("  ");
            }

            s.push_str(&generate_exp(exp, nest + 1));
            s.push_str("\n");
        }
        s
    }
}

fn generate_fn_args(declares: &Vec<Declare>) -> String {
    let mut decs = Vec::new();
    for dec in declares.iter() {
        let s = match dec {
            Declare::Var(name, Some(kw)) => match kw {
                Keyword::Float => format!("{}: float", name.0),
                Keyword::Void => format!("{}: void", name.0),
                kw => panic!("unknown type name {:?}", kw),
            },
            Declare::Var(name, None) => format!("{}", name.0),
        };
        decs.push(s);
    }

    decs.join(", ")
}

fn generate_fn_body(body: &Block, nest: u64) -> String {
    let bodystr = body
        .iter()
        .map(|s| generate_statement(s, nest + 1))
        .collect::<Vec<_>>()
        .join("");
    format!("{{\n{}}}", bodystr)
}

fn generate_exp(exp: &Exp, nest: u64) -> String {
    match exp {
        Exp::Float(f) => format!("{}", f),
        Exp::String(s) => format!("\"{}\"", s),
        Exp::Special(var) => match **var {
            Special::Now => format!("now"),
            Special::SelfVar => format!("self"),
        },
        Exp::Variable(name) => format!("{}", name.0),
        Exp::Fn(f) => format!(
            "|{}| {}",
            generate_fn_args(&f.args),
            generate_fn_body(&f.body, nest)
        ),
        Exp::InvokeFn(invoke) => generate_invoke_fn(invoke),
        Exp::UnaryOp(op, exp) => match **op {
            Operator::Minus => format!("-{}", generate_exp(exp, nest)),
            _ => panic!("unknown unary operator {:?}", op),
        },
        Exp::BinaryOp(op, exp1, exp2) => {
            let opstr = op.to_string();

            let exp1str = if let Exp::BinaryOp(_, _, _) = **exp1 {
                format!("({})", generate_exp(exp1, nest))
            } else {
                format!("{}", generate_exp(exp1, nest))
            };
            let exp2str = if let Exp::BinaryOp(_, _, _) = **exp2 {
                format!("({})", generate_exp(exp2, nest))
            } else {
                format!("{}", generate_exp(exp2, nest))
            };

            format!("{} {} {}", exp1str, opstr, exp2str)
        }
        Exp::PostOp(op, name, exp) => match **op {
            Operator::Access => format!("{}[{}]", name.0, generate_exp(exp, nest)),
            _ => panic!("unknown postfix operator {:?}", op),
        },
        Exp::If(ifexp) => {
            let true_clause = generate_if_body(&ifexp.true_clause, nest);
            let false_clause = if let Some(exp) = &ifexp.false_clause {
                format!("{}", generate_if_body(&exp, nest))
            } else {
                "{{}}".to_string()
            };
            format!(
                "if ({}) {} else {}",
                generate_exp(&ifexp.cond, nest),
                true_clause,
                false_clause,
            )
        }
    }
}

fn generate_statement(ast: &Statement, nest: u64) -> String {
    let mut indent = String::new();
    for _ in 0..nest {
        indent.push_str("  ");
    }

    let body = match ast {
        Statement::Exp(exp) => generate_exp(exp, nest),
        Statement::Assign(name, exp) => format!("{} = {}", name.0, generate_exp(exp, nest)),
        Statement::Return(exp) => format!("return {}", generate_exp(exp, nest)),
        Statement::InvokeAt(invoke, exp) => match **exp {
            Exp::Float(_) => format!("{}@{}", generate_invoke_fn(invoke), generate_exp(exp, nest)),
            Exp::Special(_) => {
                format!("{}@{}", generate_invoke_fn(invoke), generate_exp(exp, nest))
            }
            Exp::Variable(_) => {
                format!("{}@{}", generate_invoke_fn(invoke), generate_exp(exp, nest))
            }
            Exp::InvokeFn(_) => {
                format!("{}@{}", generate_invoke_fn(invoke), generate_exp(exp, nest))
            }
            Exp::BinaryOp(_, _, _) => format!(
                "{}@({})",
                generate_invoke_fn(invoke),
                generate_exp(exp, nest)
            ),
            Exp::PostOp(_, _, _) => format!(
                "{}@({})",
                generate_invoke_fn(invoke),
                generate_exp(exp, nest)
            ),
            _ => panic!("unexpected expression: {:?}", exp),
        },
    };

    format!("{}{}\n", indent, body)
}

pub fn generate_1(ast: &AST, nest: u64) -> String {
    match ast {
        AST::Statement(stmt) => generate_statement(stmt, nest),
        AST::Block(block) => {
            let mut s = String::new();
            for stmt in block.iter() {
                s.push_str(&generate_statement(stmt, nest));
                s.push('\n');
            }
            s
        }
        AST::Defun(name, f) => {
            let decstr = generate_fn_args(&f.args);
            let retstr = match &f.ret_type {
                Some(Keyword::Float) => format!("-> float"),
                Some(Keyword::Void) => format!("-> void"),
                Some(kw) => panic!("unknown type name {:?}", kw),
                _ => format!(""),
            };
            let bodystr = generate_fn_body(&f.body, nest);
            format!("fn {}({}) {} {}\n", name.0, decstr, retstr, bodystr)
        }
    }
}

pub fn generate(asts: &Vec<AST>) -> String {
    let mut code = String::new();

    for ast in asts.iter() {
        code.push_str(&generate_1(ast, 0));
    }

    code
}
