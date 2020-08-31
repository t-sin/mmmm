use crate::tokenize::Token;
use nom::error::ErrorKind;
use nom::{Err, IResult};

#[derive(Debug, PartialEq)]
pub struct Symbol(String);

#[derive(Debug, PartialEq)]
pub enum Exp {
    Float(f64),
    String(String),
    Variable(Box<Symbol>),
    InvokeFn(Box<Symbol>, Vec<Exp>),
    UnaryOp(String, Box<Exp>),
    BinOp(String, Box<Exp>, Box<Exp>),
    PostOp(String, Box<Symbol>, Box<Exp>),
}

#[derive(Debug, PartialEq)]
pub enum AST {
    Exp(Box<Exp>),
}

enum OperatorAssociativity {
    Left,
    Right,
    None,
}

/// 演算子の結合性を与える
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

/// 演算子の優先順位を与える
///
/// 数が小さいほうが優先度が高い
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
struct ParseExpState<'a> {
    nest: u32, // for debug
    input: &'a [Token<'a>],
    output: Vec<Exp>,
    stack: Vec<Token<'a>>,
    prev_token: Option<Token<'a>>,
}

fn parse_args<'a>(
    _state: &mut ParseExpState<'a>,
) -> Result<Vec<Exp>, Err<(&'a [Token<'a>], ErrorKind)>> {
    let args = Vec::new();
    Ok(args)
}

fn terminate_parse_exp_1<'a>(
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    match state.stack.pop() {
        Some(Token::Op(op)) => {
            if let (Some(exp2), Some(exp1)) = (state.output.pop(), state.output.pop()) {
                state
                    .output
                    .push(Exp::BinOp(op.to_string(), Box::new(exp1), Box::new(exp2)));
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
        let args = match parse_args(state) {
            Ok(args) => args,
            Err(err) => return Err(err),
        };
        state
            .output
            .push(Exp::InvokeFn(Box::new(Symbol(name.to_string())), args));
    } else {
        // variable
        state
            .output
            .push(Exp::Variable(Box::new(Symbol(name.to_string()))));
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

fn parse_exp_1<'a>(state: &mut ParseExpState<'a>) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    let token = state.input.iter().nth(0);
    if state.input.len() > 0 {
        state.input = &state.input[1..];
    }

    match token {
        Some(Token::CloseBrace) => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Float(f)) => state.output.push(Exp::Float(*f)),
        Some(Token::String(s)) => state.output.push(Exp::String(s.to_string())),
        Some(Token::Keyword(_)) => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Comma) => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Special(name)) => state
            .output
            .push(Exp::Variable(Box::new(Symbol(name.to_string())))),
        Some(Token::Identifier(name)) => {
            parse_exp_1_identifier(name, state);
        }
        Some(Token::OpenParen) => {
            parse_exp_1_subexp(state);
        }
        Some(Token::Op(op1)) => {
            let unary_p = match state.prev_token {
                Some(Token::Float(_)) if op1 == &"-" => false,
                Some(Token::Op("-")) if op1 == &"-" => false,
                Some(Token::Op(_)) if op1 == &"-" => true,
                Some(Token::OpenParen) if op1 == &"-" => true,
                Some(_) => false,
                None if op1 == &"-" => true,
                None => false,
            };

            if unary_p {
                state.prev_token = Some(token.unwrap().clone());
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
                            state.output.push(Exp::BinOp(
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
                state.prev_token = Some(token.unwrap().clone());
            }
        }
        Some(Token::OpenBrace)
        | Some(Token::OpenBracket)
        | Some(Token::CloseBracket)
        | Some(Token::FnReturnType)
        | Some(Token::Assign)
        | Some(Token::TimeAt) => {
            return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
        }
        Some(Token::Newline) | Some(Token::CloseParen) | None => {
            if let Err(err) = terminate_parse_exp_1(state) {
                return Err(err);
            }
        }
    }

    state.prev_token = if let Some(token) = token {
        Some(token.clone())
    } else {
        None
    };

    Ok(())
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
        Err(err) => {
            println!("{:?}", err);
            Err(err)
        }
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
    fn test_parse() {
        test_parse_1(AST::Exp(Box::new(Exp::Float(0.0))), "0.0");

        test_parse_1(
            AST::Exp(Box::new(Exp::UnaryOp(
                "-".to_string(),
                Box::new(Exp::Float(1.0)),
            ))),
            "-1",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "+".to_string(),
                Box::new(Exp::Float(1.0)),
                Box::new(Exp::Float(2.0)),
            ))),
            "1+2",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "-".to_string(),
                Box::new(Exp::Float(10.0)),
                Box::new(Exp::Float(5.0)),
            ))),
            "10-5",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "-".to_string(),
                Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(1.0)))),
                Box::new(Exp::Float(2.0)),
            ))),
            "-1-2",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "*".to_string(),
                Box::new(Exp::Float(42.0)),
                Box::new(Exp::UnaryOp("-".to_string(), Box::new(Exp::Float(1.0)))),
            ))),
            "42*-1",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "*".to_string(),
                Box::new(Exp::BinOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))),
            "(1+2)*3",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "-".to_string(),
                Box::new(Exp::BinOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))),
            "1+2-3", // (1+2)-3
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "+".to_string(),
                Box::new(Exp::BinOp(
                    "-".to_string(),
                    Box::new(Exp::BinOp(
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
            AST::Exp(Box::new(Exp::BinOp(
                "*".to_string(),
                Box::new(Exp::BinOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::BinOp(
                    "-".to_string(),
                    Box::new(Exp::Float(3.0)),
                    Box::new(Exp::Float(4.0)),
                )),
            ))),
            "(1+2)*(3-4)",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::BinOp(
                "*".to_string(),
                Box::new(Exp::BinOp(
                    "-".to_string(),
                    Box::new(Exp::BinOp(
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
            AST::Exp(Box::new(Exp::BinOp(
                "*".to_string(),
                Box::new(Exp::BinOp(
                    "+".to_string(),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::BinOp(
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
}
