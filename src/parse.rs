use crate::tokenize::Token;
use nom::error::ErrorKind;
use nom::{Err, IResult};

#[derive(Debug, PartialEq)]
/// Represents mmmm's identifier
pub struct Symbol(String);

#[derive(Debug, PartialEq)]
/// Represents mmmm's expressions.
pub enum Exp {
    Float(f64),
    String(String),
    Variable(Box<Symbol>),
    InvokeFn(Box<Symbol>, Vec<Exp>),
    UnaryOp(String, Box<Exp>),
    BinaryOp(String, Box<Exp>, Box<Exp>),
    PostOp(String, Box<Symbol>, Box<Exp>),
}

#[derive(Debug, PartialEq)]
/// Represents mmmm's abstruct syntax tree.
///
/// Note: Now can parse only expressions.
pub enum AST {
    Exp(Box<Exp>),
}

/// Represents mmmm's operator associativity.
enum OperatorAssociativity {
    Left,
    Right,
    None,
}

/// Gives associativity to the specified operator name.
fn operator_associativity(op: &str) -> OperatorAssociativity {
    match op {
        "[]" | "()" => OperatorAssociativity::None,
        "." => OperatorAssociativity::Left,
        "!" => OperatorAssociativity::Right,
        "+" | "-" | "*" | "/" | "%" => OperatorAssociativity::Left,
        "<=" | ">=" | "==" => OperatorAssociativity::Left,
        "<" | ">" => OperatorAssociativity::Left,
        "&&" | "||" => OperatorAssociativity::Left,
        _ => OperatorAssociativity::None,
    }
}

/// Gives a number of operator precedence to the specified operator name.
///
/// Lesser operator precedence number means the operators has greater precedence.
fn operator_precedence(op: &str) -> i32 {
    match op {
        "[]" | "()" => 1, // 後置演算子
        "." => 1,         // メンバアクセス演算子。まだない。
        "!" => 2,         // ないけど
        "*" | "/" | "%" => 3,
        "+" | "-" => 4,
        "<=" | ">=" | "==" => 5,
        "<" | ">" => 5,
        "&&" => 6,
        "||" => 7,
        _ => -1, // そんな演算子はないエラー
    }
}

#[derive(Debug)]
/// Structures for the state of expression parsing.
///
/// This structure's state is a state for modified shunting yard algorithm.
struct ParseExpState<'a> {
    /// A number of nesting of parse_exp_1. It's for debugging.
    nest: u32,
    /// A rest of inputs
    input: &'a [Token<'a>],
    /// An output queue.
    output: Vec<Exp>,
    /// An operator stack.
    stack: Vec<Token<'a>>,
    /// A token for the previous parse_exp_1.
    prev_token: Option<Token<'a>>,
}

fn parse_funcall_args<'a>(
    state: &mut ParseExpState<'a>,
) -> Result<Vec<Exp>, Err<(&'a [Token<'a>], ErrorKind)>> {
    let mut args = Vec::new();
    let mut separated = false;

    loop {
        match state.input.iter().nth(0) {
            Some(Token::OpenParen) => {
                state.input = &state.input[1..];
                state.prev_token = Some(Token::OpenParen);
                separated = true;
            }
            Some(Token::CloseParen) => {
                state.input = &state.input[1..];
                state.prev_token = Some(Token::CloseParen);
                break;
            }
            Some(Token::Comma) => {
                separated = true;
                state.input = &state.input[1..];
                state.prev_token = Some(Token::Comma)
            }
            Some(_) => (),
            None => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        }

        if separated {
            let mut arg_state = ParseExpState {
                nest: state.nest + 1,
                input: state.input,
                output: Vec::new(),
                stack: Vec::new(),
                prev_token: None,
            };

            if let Err(err) = parse_exp(&mut arg_state) {
                return Err(err);
            } else {
                if let Some(exp) = arg_state.output.pop() {
                    args.push(exp);
                    separated = false;
                } else {
                    return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
                }
            }
            state.input = arg_state.input;
            state.prev_token = arg_state.prev_token;
        }
    }

    Ok(args)
}

fn terminate_parse_exp_1<'a>(
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    match state.stack.pop() {
        Some(Token::Op(op)) => {
            if let (Some(exp2), Some(exp1)) = (state.output.pop(), state.output.pop()) {
                let exp = Exp::BinaryOp(op.to_string(), Box::new(exp1), Box::new(exp2));
                state.output.push(exp);
            } else {
                return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
            }
        }
        None => (),
        _ => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
    }

    Ok(())
}

fn parse_exp_1_identifier<'a>(
    name: &String,
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    if let Some(Token::OpenParen) = state.input.iter().nth(0) {
        // function invokation
        let args = match parse_funcall_args(state) {
            Ok(args) => args,
            Err(err) => return Err(err),
        };
        let exp = Exp::InvokeFn(Box::new(Symbol(name.to_string())), args);
        state.output.push(exp);
    } else {
        // variable
        let exp = Exp::Variable(Box::new(Symbol(name.to_string())));
        state.output.push(exp);
    }

    Ok(())
}

fn parse_exp_1_subexp<'a>(
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    // precedes paren expression
    let mut subexp_state = ParseExpState {
        nest: state.nest + 1,
        input: state.input,
        output: Vec::new(),
        stack: Vec::new(),
        prev_token: None,
    };

    if let Err(err) = parse_exp(&mut subexp_state) {
        return Err(err);
    }

    state.input = subexp_state.input;
    state.prev_token = subexp_state.prev_token;
    if let Some(exp) = subexp_state.output.pop() {
        state.output.push(exp);
    }

    Ok(())
}

fn is_unary<'a>(op: &str, prev_token: Option<Token<'a>>) -> bool {
    match op {
        "-" => match prev_token {
            Some(Token::Float(_)) => false,
            Some(Token::Op("-")) => false,
            Some(Token::Op(_)) => true,
            Some(Token::OpenParen) => true,
            Some(_) => false,
            None => true,
        },
        _ => false,
    }
}

fn parse_exp_1_op<'a>(
    op1: &str,
    token: Option<Token<'a>>,
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    if is_unary(op1, state.prev_token.clone()) {
        if let Err(err) = parse_exp_1(state) {
            return Err(err);
        } else {
            if let Some(exp) = state.output.pop() {
                let exp = Exp::UnaryOp(op1.to_string(), Box::new(exp));
                state.output.push(exp);
            } else {
                return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
            }
        }
    } else {
        loop {
            if let Some(Token::Op(op2)) = state.stack.last() {
                if let OperatorAssociativity::Left = operator_associativity(op1) {
                    if operator_precedence(op1) > operator_precedence(op2) {
                        break;
                    }
                } else {
                    if operator_precedence(op1) >= operator_precedence(op2) {
                        break;
                    }
                }

                if let (Some(Token::Op(op)), Some(exp2), Some(exp1)) =
                    (state.stack.pop(), state.output.pop(), state.output.pop())
                {
                    state.output.push(Exp::BinaryOp(
                        op.to_string(),
                        Box::new(exp1),
                        Box::new(exp2),
                    ));
                } else {
                    return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
                }
            } else {
                break;
            }
        }
        state.stack.push(token.unwrap().clone());
    }

    Ok(())
}

fn parse_exp_1<'a>(state: &mut ParseExpState<'a>) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    let token = state.input.iter().nth(0);
    if state.input.len() > 0 {
        state.input = &state.input[1..];
    }

    let result = match token {
        Some(Token::CloseBrace) => Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Float(f)) => {
            state.output.push(Exp::Float(*f));
            Ok(())
        }
        Some(Token::String(s)) => {
            state.output.push(Exp::String(s.to_string()));
            Ok(())
        }
        Some(Token::Keyword(_)) => Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Special(name)) => {
            state
                .output
                .push(Exp::Variable(Box::new(Symbol(name.to_string()))));
            Ok(())
        }
        Some(Token::Identifier(name)) => parse_exp_1_identifier(name, state),
        Some(Token::OpenParen) => parse_exp_1_subexp(state),
        Some(Token::Op(op1)) => {
            let result = parse_exp_1_op(op1, Some(token.unwrap().clone()), state);
            state.prev_token = Some(token.unwrap().clone());
            result
        }
        Some(Token::OpenBrace)
        | Some(Token::OpenBracket)
        | Some(Token::CloseBracket)
        | Some(Token::FnReturnType)
        | Some(Token::Assign)
        | Some(Token::TimeAt) => Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Comma) => Ok(()),
        Some(Token::Newline) | Some(Token::CloseParen) | None => terminate_parse_exp_1(state),
    };

    state.prev_token = if let Some(token) = token {
        Some(token.clone())
    } else {
        None
    };

    result
}

/// 操車場アルゴリズムでトークン列から式オブジェクトを構築する
fn parse_exp<'a>(state: &mut ParseExpState<'a>) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    while state.input.len() > 0 {
        if let Err(err) = parse_exp_1(state) {
            return Err(err);
        }

        if let Some(Token::CloseParen) = state.prev_token {
            break;
        }
    }
    if let Err(err) = terminate_parse_exp_1(state) {
        return Err(err);
    }

    Ok(())
}

fn parse_expression<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {
    let mut state = ParseExpState {
        nest: 0,
        input: t,
        output: Vec::new(),
        stack: Vec::new(),
        prev_token: None,
    };

    match parse_exp(&mut state) {
        Ok(()) => {
            if let Some(exp) = state.output.pop() {
                Ok((state.input, Some(AST::Exp(Box::new(exp)))))
            } else {
                Ok((state.input, None))
            }
        }
        Err(err) => Err(err),
    }
}

fn parse_1<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {
    parse_expression(t)
}

pub fn parse<'a>(t: &'a [Token]) -> IResult<&'a [Token<'a>], Vec<AST>> {
    let mut asts = Vec::new();
    let mut input = t;

    loop {
        if input.len() == 0 {
            break;
        }

        match parse_1(input) {
            Ok((t, Some(ast))) => {
                input = t;
                asts.push(ast);
            }
            Ok((t, None)) => {
                input = t;
            }
            Err(err) => return Err(err),
        }
    }

    Ok((input, asts))
}

#[cfg(test)]
mod test_parse {
    use super::*;
    use crate::tokenize::tokenize;

    fn test_parse_1(expected: AST, string: &str) {
        println!("text: {:?}", string);
        if let Ok(("", tokens)) = tokenize(string) {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(1, vec.len());

                    if let Some(ast) = vec.iter().nth(0) {
                        assert_eq!(*ast, expected);
                    } else {
                        println!("This test case itself is wrong....");
                        assert!(false);
                    }
                }
                err => {
                    println!("{:?}", err);
                    assert!(false);
                }
            }
        } else {
            println!("This test case itself is wrong....");
            assert!(false);
        }
    }

    #[test]
    fn test_single_term() {
        test_parse_1(AST::Exp(Box::new(Exp::Float(0.0))), "0.0");

        test_parse_1(
            AST::Exp(Box::new(Exp::UnaryOp(
                "-".to_string(),
                Box::new(Exp::Float(1.0)),
            ))),
            "-1",
        );
    }

    #[test]
    fn test_binary_operators() {
        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "+".to_string(),
                Box::new(Exp::Float(1.0)),
                Box::new(Exp::Float(2.0)),
            ))),
            "1+2",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "-".to_string(),
                Box::new(Exp::Float(10.0)),
                Box::new(Exp::Float(5.0)),
            ))),
            "10-5",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "-".to_string(),
                Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(1.0)))),
                Box::new(Exp::Float(2.0)),
            ))),
            "-1-2",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "*".to_string(),
                Box::new(Exp::Float(42.0)),
                Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(1.0)))),
            ))),
            "42*-1",
        );
    }

    #[test]
    fn test_nested_expressions() {
        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "*".to_string(),
                Box::new(Exp::BinaryOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))),
            "(1+2)*3",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "-".to_string(),
                Box::new(Exp::BinaryOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))),
            "1+2-3", // (1+2)-3
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "+".to_string(),
                Box::new(Exp::BinaryOp(
                    "-".to_string(),
                    Box::new(Exp::BinaryOp(
                        "+".to_string(),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    )),
                    Box::new(Exp::Float(3.0)),
                )),
                Box::new(Exp::Float(4.0)),
            ))),
            "1+2-3+4", // ((1+2)-3)+4
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "*".to_string(),
                Box::new(Exp::BinaryOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::BinaryOp(
                    "-".to_string(),
                    Box::new(Exp::Float(3.0)),
                    Box::new(Exp::Float(4.0)),
                )),
            ))),
            "(1+2)*(3-4)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "*".to_string(),
                Box::new(Exp::BinaryOp(
                    "-".to_string(),
                    Box::new(Exp::BinaryOp(
                        "+".to_string(),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(2.0)))),
                    )),
                    Box::new(Exp::Float(3.0)),
                )),
                Box::new(Exp::Float(4.0)),
            ))),
            "(1+-2-3)*4",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "*".to_string(),
                Box::new(Exp::BinaryOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::BinaryOp(
                        "-".to_string(),
                        Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(2.0)))),
                        Box::new(Exp::Float(3.0)),
                    )),
                )),
                Box::new(Exp::Float(4.0)),
            ))),
            "(1+(-2-3))*4",
        );
    }

    #[test]
    fn test_function_call() {
        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("fnc".to_string())),
                vec![Exp::Float(1.0)],
            ))),
            "fnc(1)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("fnc".to_string())),
                vec![Exp::Float(1.0), Exp::Float(2.0), Exp::Float(3.0)],
            ))),
            "fnc(1,2,3)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinaryOp(
                "+".to_string(),
                Box::new(Exp::InvokeFn(
                    Box::new(Symbol("fn1".to_string())),
                    vec![Exp::Float(1.0), Exp::Float(2.0), Exp::Float(3.0)],
                )),
                Box::new(Exp::InvokeFn(
                    Box::new(Symbol("fn2".to_string())),
                    vec![Exp::Float(1.0)],
                )),
            ))),
            "fn1(1,2,3) + fn2(1)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("f1".to_string())),
                vec![Exp::InvokeFn(
                    Box::new(Symbol("f2".to_string())),
                    vec![Exp::Float(42.0)],
                )],
            ))),
            "f1(f2(42))",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("f1".to_string())),
                vec![
                    Exp::InvokeFn(Box::new(Symbol("f2".to_string())), vec![Exp::Float(10.0)]),
                    Exp::InvokeFn(Box::new(Symbol("f3".to_string())), vec![Exp::Float(20.0)]),
                ],
            ))),
            "f1(f2(10), f3(20))",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("f1".to_string())),
                vec![
                    Exp::InvokeFn(Box::new(Symbol("f2".to_string())), vec![Exp::Float(10.0)]),
                    Exp::InvokeFn(Box::new(Symbol("f3".to_string())), vec![Exp::Float(20.0)]),
                ],
            ))),
            "f1(f2(10), f3(20))",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("f1".to_string())),
                vec![Exp::BinaryOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )],
            ))),
            "f1(1+2)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::InvokeFn(
                Box::new(Symbol("f1".to_string())),
                vec![
                    Exp::BinaryOp(
                        "+".to_string(),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    ),
                    Exp::BinaryOp(
                        "*".to_string(),
                        Box::new(Exp::Float(3.0)),
                        Box::new(Exp::Float(4.0)),
                    ),
                ],
            ))),
            "f1(1+2, 3*4)",
        );
    }
}
