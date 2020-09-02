use crate::tokenize::{token_type_eq, Token};
use nom::branch::{alt, permutation};
use nom::combinator::{opt, rest_len};
use nom::error::ErrorKind;
use nom::multi::separated_list;
use nom::sequence::delimited;
use nom::{Err, IResult};

/// Recognize one token.
fn token<'a>(token: Token<'a>) -> impl Fn(&'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a Token> {
    move |t| {
        let (t, len) = rest_len(t)?;
        if len != 0 {
            if &t[0] == &token {
                Ok((&t[1..], &t[0]))
            } else {
                Err(Err::Error((&t[..], ErrorKind::IsNot)))
            }
        } else {
            Err(Err::Error((&t[..], ErrorKind::Eof)))
        }
    }
}

/// Recognize one token by checking token type.
///
/// It means this combinator ignores enum variants' union value.
fn token_type_of<'a>(
    token: Token<'a>,
) -> impl Fn(&'a [Token<'a>]) -> IResult<&'a [Token<'a>], &'a Token> {
    move |t| {
        let (t, len) = rest_len(t)?;
        if len != 0 {
            if token_type_eq(&t[0], &token) {
                Ok((&t[1..], &t[0]))
            } else {
                Err(Err::Error((&t[..], ErrorKind::IsNot)))
            }
        } else {
            Err(Err::Error((&t[..], ErrorKind::Eof)))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's identifier
pub struct Symbol(String);

#[derive(Debug, PartialEq, Clone)]
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
    Assign(Box<Symbol>, Box<Exp>),
    Defun(Box<Symbol>, Vec<Symbol>, Option<Symbol>, Vec<AST>),
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
    loop {
        match state.stack.pop() {
            Some(Token::Op(op)) => {
                if let (Some(exp2), Some(exp1)) = (state.output.pop(), state.output.pop()) {
                    let exp = Exp::BinaryOp(op.to_string(), Box::new(exp1), Box::new(exp2));
                    state.output.push(exp);
                } else {
                    return Err(Err::Error((&state.input[..], ErrorKind::IsNot)));
                }
            }
            None => break,
            _ => return Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        }
    }

    Ok(())
}

fn parse_array_access<'a>(
    state: &mut ParseExpState<'a>,
) -> Result<Exp, Err<(&'a [Token<'a>], ErrorKind)>> {
    state.input = &state.input[1..];
    state.prev_token = Some(Token::OpenBracket);

    let mut array_access_state = ParseExpState {
        nest: state.nest + 1,
        input: state.input,
        output: Vec::new(),
        stack: Vec::new(),
        prev_token: None,
    };

    if let Err(err) = parse_exp(&mut array_access_state) {
        return Err(err);
    }

    if let Some(Token::CloseBracket) = array_access_state.input.iter().nth(0) {
        state.input = &array_access_state.input[1..];
        state.prev_token = Some(Token::CloseBracket);
        if let Some(exp) = array_access_state.output.pop() {
            Ok(exp)
        } else {
            Err(Err::Error((
                &array_access_state.input[..],
                ErrorKind::IsNot,
            )))
        }
    } else {
        Err(Err::Error((
            &array_access_state.input[..],
            ErrorKind::IsNot,
        )))
    }
}

fn parse_exp_1_identifier<'a>(
    name: &String,
    state: &mut ParseExpState<'a>,
) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    match state.input.iter().nth(0) {
        Some(Token::OpenParen) => {
            // function invokation
            let args = match parse_funcall_args(state) {
                Ok(args) => args,
                Err(err) => return Err(err),
            };
            let exp = Exp::InvokeFn(Box::new(Symbol(name.to_string())), args);
            state.output.push(exp);
        }
        Some(Token::OpenBracket) => match parse_array_access(state) {
            Ok(exp) => {
                let exp = Exp::PostOp(
                    "[]".to_string(),
                    Box::new(Symbol(name.to_string())),
                    Box::new(exp),
                );
                state.output.push(exp);
            }
            Err(err) => return Err(err),
        },
        _ => {
            // variable
            let exp = Exp::Variable(Box::new(Symbol(name.to_string())));
            state.output.push(exp);
        }
    };

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

    if let Some(Token::CloseParen) = subexp_state.input.iter().nth(0) {
        if let Some(exp) = subexp_state.output.pop() {
            state.output.push(exp);
        }
        state.input = &subexp_state.input[1..];
        state.prev_token = Some(Token::CloseParen);
        Ok(())
    } else {
        Err(Err::Error((&subexp_state.input[..], ErrorKind::IsNot)))
    }
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
                // if op2 has high precedence (lesser number) than op1, immediately pop op2 and construct Exp::BinaryOp
                // so op1 has lesser precedence number (it means low precedence), immediately breaks this loop
                match operator_associativity(op1) {
                    OperatorAssociativity::Left => {
                        if operator_precedence(op1) < operator_precedence(op2) {
                            break;
                        }
                    }
                    OperatorAssociativity::Right => {
                        if operator_precedence(op1) <= operator_precedence(op2) {
                            break;
                        }
                    }
                    OperatorAssociativity::None => {
                        panic!("oh {:?} is not an operator. it may be a bug!", op1);
                    }
                };

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
        Some(Token::LineComment(_)) => {
            // now simply discards comment token
            state.input = &state.input[1..];
            // state.prev_token = Some(token.unwrap().clone());
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
        | Some(Token::FnReturnType)
        | Some(Token::Assign)
        | Some(Token::TimeAt) => Err(Err::Error((&state.input[..], ErrorKind::IsNot))),
        Some(Token::Comma) => Ok(()),
        Some(Token::Colon) => Err(Err::Error((&state.input[1..], ErrorKind::IsNot))),
        Some(Token::Newline) | Some(Token::CloseParen) | Some(Token::CloseBracket) | None => {
            terminate_parse_exp_1(state)
        }
    };

    state.prev_token = if let Some(token) = token {
        Some(token.clone())
    } else {
        None
    };

    result
}

fn end_of_exp<'a>(state: &mut ParseExpState<'a>) -> bool {
    if state.input.len() == 0 {
        return true;
    }

    match state.input.iter().nth(0).unwrap() {
        Token::Keyword(_) => true,
        Token::Comma => true,
        Token::Newline => true,
        Token::CloseParen => true,
        Token::CloseBracket => true,
        Token::OpenBrace | Token::CloseBrace => true,
        _ => false,
    }
}

/// 操車場アルゴリズムでトークン列から式オブジェクトを構築する
fn parse_exp<'a>(state: &mut ParseExpState<'a>) -> Result<(), Err<(&'a [Token<'a>], ErrorKind)>> {
    while !end_of_exp(state) {
        if let Err(err) = parse_exp_1(state) {
            return Err(err);
        }
    }

    if let Err(err) = terminate_parse_exp_1(state) {
        return Err(err);
    }

    if let Some(Token::Newline) = state.input.iter().nth(0) {
        state.input = &state.input[1..];
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

fn parse_assignment<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {
    match permutation((
        token_type_of(Token::Identifier("".to_string())),
        token_type_of(Token::Assign),
        parse_expression,
    ))(t)
    {
        Ok((rest, (Token::Identifier(name), _, ref ast))) => {
            if let Some(AST::Exp(exp)) = ast {
                let ast = AST::Assign(Box::new(Symbol(name.to_string())), (*exp).clone());
                Ok((rest, Some(ast)))
            } else {
                Err(Err::Error((rest, ErrorKind::IsNot)))
            }
        }
        Ok((rest, (_, _, _))) => Err(Err::Error((rest, ErrorKind::IsNot))),
        Err(err) => Err(err),
    }
}

//fn parse_function_args<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {}

fn parse_function_definition<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {
    match permutation((
        token(Token::Keyword("fn")),
        token_type_of(Token::Identifier("".to_string())),
        // parse function args
        delimited(
            token(Token::OpenParen),
            separated_list(
                token(Token::Comma),
                token_type_of(Token::Identifier("".to_string())),
            ),
            token(Token::CloseParen),
        ),
        // parse function return type
        opt(permutation((
            token(Token::FnReturnType),
            token_type_of(Token::Keyword("")),
        ))),
        // parse function body
        delimited(
            token(Token::OpenBrace),
            parse_expression,
            token(Token::CloseBrace),
        ),
    ))(t)
    {
        Ok((rest, (_, fn_name, args, fn_type, expvec))) => Ok((
            rest,
            Some(AST::Defun(
                // function name
                if let Token::Identifier(name) = fn_name {
                    Box::new(Symbol(name.to_string()))
                } else {
                    return Err(Err::Error((rest, ErrorKind::IsNot)));
                },
                // function args
                args.into_iter()
                    .map(|t| {
                        if let Token::Identifier(name) = t {
                            Symbol(name.to_string())
                        } else {
                            panic!("unreached here because of matching Token::Identifier")
                        }
                    })
                    .collect(),
                // function return type
                if let Some((_, Token::Keyword(type_name))) = fn_type {
                    Some(Symbol(type_name.to_string()))
                } else {
                    None
                },
                // function body
                vec![expvec.unwrap()],
                // expvec
                //     .into_iter()
                //     .filter(|o| if let None = o { false } else { true })
                //     .map(|o| o.unwrap())
                //     .collect(),
            )),
        )),
        Err(err) => Err(err),
    }
}

fn parse_1<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], Option<AST>> {
    let (t, ast) = alt((
        parse_function_definition,
        parse_assignment,
        parse_expression,
    ))(t)?;
    Ok((t, ast))
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
                Box::new(Exp::Float(1.0)),
                Box::new(Exp::BinaryOp(
                    "*".to_string(),
                    Box::new(Exp::Float(2.0)),
                    Box::new(Exp::Float(3.0)),
                )),
            ))),
            "1+2*3", // 1+(2*3)
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
    fn test_array_access() {
        test_parse_1(
            AST::Exp(Box::new(Exp::PostOp(
                "[]".to_string(),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::Float(1.0)),
            ))),
            "a[1]",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::PostOp(
                "[]".to_string(),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::Variable(Box::new(Symbol("var".to_string())))),
            ))),
            "a[var]",
        );

        test_parse_1(
            AST::Exp(Box::new(Exp::PostOp(
                "[]".to_string(),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::BinaryOp(
                    "%".to_string(),
                    Box::new(Exp::Variable(Box::new(Symbol("var".to_string())))),
                    Box::new(Exp::BinaryOp(
                        "-".to_string(),
                        Box::new(Exp::Float(10.0)),
                        Box::new(Exp::Float(2.0)),
                    )),
                )),
            ))),
            "a[var%(10-2)]",
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

    fn test_parse_all(expected: &[AST], string: &str) {
        println!("text: {:?}", string);
        if let Ok(("", tokens)) = tokenize(string) {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(vec.len(), expected.len());
                    for (i, ast) in vec.iter().enumerate() {
                        assert_eq!(*ast, expected[i]);
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
    fn test_assignment() {
        test_parse_all(
            &[AST::Assign(
                Box::new(Symbol("var".to_string())),
                Box::new(Exp::Float(123.0)),
            )],
            "var = 123",
        );

        test_parse_all(
            &[
                AST::Assign(
                    Box::new(Symbol("var".to_string())),
                    Box::new(Exp::Float(123.0)),
                ),
                AST::Assign(
                    Box::new(Symbol("var2".to_string())),
                    Box::new(Exp::Float(456.0)),
                ),
            ],
            "var = 123
             var2 = 456",
        );

        test_parse_all(
            &[
                AST::Assign(
                    Box::new(Symbol("var".to_string())),
                    Box::new(Exp::Float(123.0)),
                ),
                AST::Assign(
                    Box::new(Symbol("var2".to_string())),
                    Box::new(Exp::BinaryOp(
                        "+".to_string(),
                        Box::new(Exp::Float(4.0)),
                        Box::new(Exp::BinaryOp(
                            "*".to_string(),
                            Box::new(Exp::Float(5.0)),
                            Box::new(Exp::Float(6.0)),
                        )),
                    )),
                ),
            ],
            "var = 123
             var2 = 4+5*6",
        );

        test_parse_all(
            &[AST::Assign(
                Box::new(Symbol("var".to_string())),
                Box::new(Exp::InvokeFn(
                    Box::new(Symbol("f".to_string())),
                    vec![Exp::BinaryOp(
                        "+".to_string(),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    )],
                )),
            )],
            "var = f(1+2)",
        );
    }
}
