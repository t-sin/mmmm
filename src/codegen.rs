use crate::parse::{Declare, Exp, InvokeFn, AST};
use crate::tokenize::{Keyword, Operator, Special};

fn generate_invoke_fn(invoke: &InvokeFn) -> String {
    let argstr = invoke
        .1
        .iter()
        .map(|e| generate_exp(e, 0))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({})", invoke.0.0, argstr)
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
            let true_clause = if ifexp.true_clause.len() > 1 {
                let mut s = String::new();
                s.push_str("\n");

                for exp in ifexp.true_clause.iter() {
                    for _ in 0..nest + 1 {
                        s.push_str("  ");
                    }

                    s.push_str(&generate_exp(exp, nest + 1));
                    s.push_str("\n");
                }
                s
            } else {
                format!(" {} ", generate_exp(&ifexp.true_clause[0], nest))
            };
            format!(
                "if ({}) {{{}}}",
                generate_exp(&ifexp.cond, nest),
                true_clause
            )
        }
    }
}

fn generate_declares(declares: &Vec<Declare>) -> String {
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

fn generate_1(ast: &AST, nest: u64) -> String {
    let mut indent = String::new();
    for _ in 0..nest {
        indent.push_str("  ");
    }

    let body = match ast {
        AST::Exp(exp) => generate_exp(exp, nest),
        AST::Assign(name, exp) => format!("{} = {}", name.0, generate_exp(exp, nest)),
        AST::Return(exp) => format!("return {}", generate_exp(exp, nest)),
        AST::InvokeAt(invoke, exp) => {
            format!("{}@{}", generate_invoke_fn(invoke), generate_exp(exp, nest))
        }
        AST::Defun(name, declares, rettype, body) => {
            let decstr = generate_declares(declares);
            let retstr = match rettype {
                Some(Keyword::Float) => format!("-> float"),
                Some(Keyword::Void) => format!("-> void"),
                Some(kw) => panic!("unknown type name {:?}", kw),
                _ => format!(""),
            };
            let bodystr = body
                .iter()
                .map(|a| generate_1(a, nest + 1))
                .collect::<Vec<_>>()
                .join("");
            format!("fn {}({}) {} {{\n{}}}", name.0, decstr, retstr, bodystr)
        }
    };

    format!("{}{}\n", indent, body)
}

pub fn generate(asts: &Vec<AST>) -> String {
    let mut code = String::new();

    for ast in asts.iter() {
        code.push_str(&generate_1(ast, 0));
    }

    code
}
