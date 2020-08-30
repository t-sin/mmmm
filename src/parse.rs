use std::collections::VecDeque;

use nom::branch::{alt, permutation};
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, multispace0, newline, none_of, one_of};
use nom::combinator::opt;
use nom::error::ErrorKind;
use nom::multi::{many0, many1};
use nom::sequence::tuple;
use nom::{Err, IResult};

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Float(f64),
    Keyword(&'a str),
    Special(&'a str),
    Op(&'a str),
    UnaryOp(&'a str),
    BinaryOp(&'a str),
    Identifier(String),
    String(String),
    Comma,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    FnReturnType,
    Assign,
    TimeAt,
    Newline,
}

fn tokenize_int(s: &str) -> IResult<&str, f64> {
    match digit1::<&str, (&str, ErrorKind)>(s) {
        Ok((s, "")) => Err(Err::Error((s, ErrorKind::Digit))),
        Ok((s, int)) => {
            if let Ok(int) = int.parse::<f64>() {
                Ok((s, int))
            } else {
                Err(Err::Error((s, ErrorKind::Digit)))
            }
        }
        _ => Err(Err::Error((s, ErrorKind::Digit))),
    }
}

fn tokenize_fract(s: &str) -> IResult<&str, f64> {
    let (s, (_, frac)) = permutation((char('.'), digit1))(s)?;
    let frac = frac.parse::<f64>().unwrap() / 10f64.powf(frac.len() as f64);
    Ok((s, frac))
}

fn tokenize_float(s: &str) -> IResult<&str, Token> {
    if s == "" {
        return Err(Err::Error((s, ErrorKind::Eof)));
    }

    let (s, int) = match tokenize_int(s) {
        Ok((s, int)) => (s, int),
        _ => return Err(Err::Error((s, ErrorKind::Digit))),
    };
    match opt(tokenize_fract)(s) {
        Ok((s, Some(frac))) => {
            let float = int + frac;
            Ok((s, Token::Float(float)))
        }
        Ok((s, None)) => Ok((s, Token::Float(int))),
        _err => Err(Err::Error((s, ErrorKind::Float))),
    }
}

fn tokenize_keyword(s: &str) -> IResult<&str, Token> {
    let (s, name) = alt((
        tag("fn"),
        tag("return"),
        tag("if"),
        tag("else"),
        tag("float"),
        tag("void"),
    ))(s)?;
    Ok((s, Token::Keyword(name)))
}

fn tokenize_special_variable(s: &str) -> IResult<&str, Token> {
    let (s, op) = alt((tag("now"), tag("self")))(s)?;
    Ok((s, Token::Special(op)))
}

fn tokenize_op(s: &str) -> IResult<&str, Token> {
    let (s, op) = alt((
        tag("<="),
        tag(">="),
        tag("=="),
        tag("+"),
        tag("-"),
        tag("*"),
        tag("/"),
        tag("%"),
        tag("<"),
        tag(">"),
        tag("！"),
    ))(s)?;
    Ok((s, Token::Op(op)))
}

fn tokenize_comma(s: &str) -> IResult<&str, Token> {
    let (s, _) = char(',')(s)?;
    Ok((s, Token::Comma))
}

fn tokenize_assignment(s: &str) -> IResult<&str, Token> {
    let (s, _) = char('=')(s)?;
    Ok((s, Token::Assign))
}

fn tokenize_time_at(s: &str) -> IResult<&str, Token> {
    let (s, _) = char('@')(s)?;
    Ok((s, Token::TimeAt))
}

fn tokenize_fn_return_type(s: &str) -> IResult<&str, Token> {
    let (s, _) = tag("->")(s)?;
    Ok((s, Token::FnReturnType))
}

fn tokenize_string(s: &str) -> IResult<&str, Token> {
    let (s, (_, string, _)) = tuple((char('"'), many1(none_of("\"")), char('"')))(s)?;
    let string: String = string.iter().collect();
    Ok((s, Token::String(string)))
}

fn tokenize_parens(s: &str) -> IResult<&str, Token> {
    let (s, token) = match alt((
        char('('),
        char(')'),
        char('['),
        char(']'),
        char('{'),
        char('}'),
    ))(s)?
    {
        (s, '(') => (s, Token::OpenParen),
        (s, ')') => (s, Token::CloseParen),
        (s, '[') => (s, Token::OpenBracket),
        (s, ']') => (s, Token::CloseBracket),
        (s, '{') => (s, Token::OpenBrace),
        (s, '}') => (s, Token::CloseBrace),
        (s, _) => return Err(Err::Error((s, ErrorKind::Char))),
    };
    Ok((s, token))
}

fn tokenize_identifier(s: &str) -> IResult<&str, Token> {
    let digits = "0123456789";
    let alpha = "abcdefghijklmnopqrstuvwxyz";
    let alpha_cap = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let underbar = "_";
    let first_chars = [alpha, alpha_cap, underbar].concat();
    let rest_chars = [digits, alpha, alpha_cap, underbar].concat();

    let (s, (first, rest)) = permutation((
        many1(one_of(&first_chars[..])),
        many0(one_of(&rest_chars[..])),
    ))(s)?;
    let first: String = first.into_iter().collect();
    let rest: String = rest.into_iter().collect();
    Ok((s, Token::Identifier([first, rest].concat())))
}

fn tokenize_newline(s: &str) -> IResult<&str, Token> {
    let (s, _) = newline(s)?;
    Ok((s, Token::Newline))
}

pub fn tokenize(s: &str) -> IResult<&str, Vec<Token>> {
    let mut tokens = Vec::new();
    let mut input = s;

    loop {
        let (s, _) = opt(multispace0::<&str, (&str, ErrorKind)>)(input)?;
        input = s;

        if s == "" {
            break;
        }

        let (s, token) = alt((
            tokenize_float,
            tokenize_fn_return_type,
            tokenize_op,
            tokenize_special_variable,
            tokenize_keyword,
            tokenize_comma,
            tokenize_parens,
            tokenize_string,
            tokenize_assignment,
            tokenize_time_at,
            tokenize_identifier,
            tokenize_newline,
        ))(input)?;
        input = s;
        tokens.push(token);
    }

    Ok((input, tokens))
}

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

fn parse_float<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], AST> {
    if t != &[] {
        match t[0] {
            Token::Float(f) => Ok((&t[1..], AST::Exp(Box::new(Exp::Float(f))))),
            _ => Err(Err::Error((t, ErrorKind::Float))),
        }
    } else {
        Err(Err::Error((t, ErrorKind::Float)))
    }
}

enum OperatorAssociativity {
    Left,
    Right,
    None,
}

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

/// 演算子の優先順位を決定する
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

/// 操車場アルゴリズムでトークン列を逆ポーランド記法に並び換える
fn shunting_yard<'a>(
    t: &'a [Token<'a>],
) -> Result<(&'a [Token<'a>], Vec<Token>), Err<(&'a [Token<'a>], ErrorKind)>> {
    let mut input = t;
    let mut output: VecDeque<Token<'a>> = VecDeque::new();
    let mut stack: Vec<Token<'a>> = Vec::new();
    let mut prev_token = None;

    'outer: loop {
        let token = input.iter().nth(0);
        let prev_input = input;
        if input.len() > 0 {
            input = &input[1..]
        }

        match token {
            Some(Token::CloseBrace) => break,
            Some(Token::Float(_)) => output.push_back(token.unwrap().clone()),
            Some(Token::String(_)) => output.push_back(token.unwrap().clone()),
            Some(Token::Keyword(_)) => return Err(Err::Error((t, ErrorKind::IsNot))),
            Some(Token::Special(_)) => output.push_back(token.unwrap().clone()),
            Some(Token::Identifier(_)) => {
                if let Some(Token::OpenParen) = input.iter().nth(0) {
                    // function invokation
                    stack.push(token.unwrap().clone());
                } else {
                    // variable
                    output.push_back(token.unwrap().clone());
                }
            }
            Some(Token::Comma) => loop {
                let _ = input.iter().nth(0);
                if input.len() > 0 {
                    input = &input[1..];
                }

                if let Some(Token::OpenParen) = stack.last() {
                    if let Some(top) = stack.pop() {
                        output.push_back(top.clone());
                        break;
                    } else {
                        return Err(Err::Error((t, ErrorKind::IsNot)));
                    }
                }
            },
            Some(Token::UnaryOp(_op)) => {
                panic!("unary operator does not have textual representation.")
            }
            Some(Token::BinaryOp(_op)) => {
                panic!("binary operator does not have textual representation.")
            }
            Some(Token::Op(op1)) => {
                let unary_p = match prev_token {
                    Some(Token::Float(_)) if op1 == &"-" => false,
                    Some(Token::Op(_)) if op1 == &"-" => true,
                    Some(_) => false,
                    None => true,
                };

                if unary_p {
                    let next_token = input.iter().nth(0);
                    if input.len() > 0 {
                        input = &input[1..]
                    }
                    output.push_back(next_token.unwrap().clone());
                    output.push_back(Token::UnaryOp(op1));
                    prev_token = Some(token.unwrap().clone());
                    continue;
                }

                loop {
                    if let Some(Token::Op(op2)) = stack.last() {
                        if let OperatorAssociativity::Left = operator_associativity(op1) {
                            if operator_precedence(op1) <= operator_precedence(op2) {
                                output.push_back(Token::BinaryOp(op2));
                            } else {
                                break;
                            }
                        } else {
                            if operator_precedence(op1) < operator_precedence(op2) {
                                output.push_back(Token::BinaryOp(op2));
                            } else {
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
                stack.push(Token::BinaryOp(op1));
            }
            Some(Token::OpenParen) => stack.push(token.unwrap().clone()),
            Some(Token::CloseParen) => loop {
                let token = input.iter().nth(0);
                if input.len() > 0 {
                    input = &input[1..]
                }
                if let Some(Token::OpenParen) = token {
                    if let None = stack.pop() {
                        return Err(Err::Error((t, ErrorKind::IsNot)));
                    }

                    let token = input.iter().nth(0);
                    if input.len() > 0 {
                        input = &input[1..]
                    }

                    if let Some(Token::Identifier(_)) = token {
                        output.push_back(token.unwrap().clone());
                        break;
                    } else {
                        return Err(Err::Error((t, ErrorKind::IsNot)));
                    }
                } else {
                    break;
                }
            },
            Some(Token::OpenBrace)
            | Some(Token::OpenBracket)
            | Some(Token::CloseBracket)
            | Some(Token::FnReturnType)
            | Some(Token::Assign)
            | Some(Token::TimeAt) => {
                return Err(Err::Error((t, ErrorKind::IsNot)));
            }
            Some(Token::Newline) | None => loop {
                if stack.len() == 0 {
                    break 'outer;
                }

                if let Some(Token::OpenParen) | Some(Token::CloseParen) = stack.last() {
                    return Err(Err::Error((t, ErrorKind::IsNot)));
                } else {
                    output.push_back(stack.pop().unwrap().clone())
                }
            },
        };

        prev_token = Some(token.unwrap().clone());
    }

    return Ok((&input[..], Vec::from(output)));
}

fn parse_expression<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], AST> {
    let rest;
    let tokens;
    match shunting_yard(t) {
        Ok((t, rpn_tokens)) => {
            rest = t;
            tokens = rpn_tokens;
        }
        Err(err) => return Err(err),
    }

    let mut stack = Vec::new();
    for t in tokens.iter() {
        let exp = match t {
            Token::Float(f) => AST::Exp(Box::new(Exp::Float(*f))),
            Token::String(s) => AST::Exp(Box::new(Exp::String(s.to_string()))),
            Token::Special(name) => {
                AST::Exp(Box::new(Exp::Variable(Box::new(Symbol(name.to_string())))))
            }
            Token::UnaryOp(name) => {
                if let Some(AST::Exp(exp)) = stack.pop() {
                    AST::Exp(Box::new(Exp::UnaryOp(name.to_string(), exp)))
                } else {
                    return Err(Err::Error((rest, ErrorKind::IsNot)));
                }
            }
            Token::BinaryOp(name) => {
                if let (Some(AST::Exp(exp2)), Some(AST::Exp(exp1))) = (stack.pop(), stack.pop()) {
                    AST::Exp(Box::new(Exp::BinOp(name.to_string(), exp1, exp2)))
                } else {
                    return Err(Err::Error((rest, ErrorKind::IsNot)));
                }
            }
            _ => return Err(Err::Error((rest, ErrorKind::IsNot))),
        };

        stack.push(exp);
    }

    Ok((rest, stack.pop().unwrap()))
}

fn parse_1<'a>(t: &'a [Token]) -> IResult<&'a [Token<'a>], AST> {
    parse_expression(t)
}

pub fn parse<'a>(t: &'a [Token]) -> IResult<&'a [Token<'a>], Vec<AST>> {
    let mut asts = Vec::new();
    let mut input = t;

    loop {
        if input.len() == 0 {
            break;
        }

        let (t, ast) = parse_1(input)?;
        input = t;
        asts.push(ast);
    }

    Ok((input, asts))
}

#[cfg(test)]
mod test_tokenize {
    use super::*;

    type TokenizeFn = dyn Fn(&str) -> IResult<&str, Token>;

    fn test_tokenize_fn(tokenize_fn: &TokenizeFn, expected: Token, input: &str) {
        if let Ok(("", result)) = tokenize_fn(input) {
            assert_eq!(expected, result);
        } else {
            println!("result = {:?}", tokenize_fn(input));
            assert!(false);
        }
    }

    fn test_tokenize_fn_ne(tokenize_fn: &TokenizeFn, unexpected: Token, input: &str) {
        if let Ok(("", result)) = tokenize_fn(input) {
            assert_ne!(unexpected, result);
        } else {
            println!("result = {:?}", tokenize_fn(input));
            assert!(false);
        }
    }

    fn test_tokenize_fn_with_error(tokenize_fn: &TokenizeFn, input: &str) {
        if let Ok(("", _)) = tokenize_fn(input) {
            println!("result = {:?}", tokenize_fn(input));
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_tokenize_float_for_empty() {
        test_tokenize_fn_with_error(&tokenize_float, "");
    }

    #[test]
    fn test_invalid_float() {
        test_tokenize_fn_with_error(&tokenize_float, ".0");
        test_tokenize_fn_with_error(&tokenize_float, ".012");
        test_tokenize_fn_with_error(&tokenize_float, "-.0");
    }

    #[test]
    fn test_tokenize_float() {
        test_tokenize_fn(&tokenize_float, Token::Float(0.0), "0");
        test_tokenize_fn(&tokenize_float, Token::Float(0.0), "0.0");

        test_tokenize_fn(&tokenize_float, Token::Float(1.0), "1");
        test_tokenize_fn(&tokenize_float, Token::Float(123.0), "123");

        test_tokenize_fn(&tokenize_float, Token::Float(123.0), "123.0");
        test_tokenize_fn(&tokenize_float, Token::Float(123.012), "123.012");

        test_tokenize_fn_with_error(&tokenize_float, "-0");
        test_tokenize_fn_with_error(&tokenize_float, "-1");
        test_tokenize_fn_with_error(&tokenize_float, "-1.0");
        test_tokenize_fn_with_error(&tokenize_float, "-127");
    }

    #[test]
    fn test_tokenize_op() {
        test_tokenize_fn(&tokenize_op, Token::Op("+"), "+");
        test_tokenize_fn(&tokenize_op, Token::Op("-"), "-");
        test_tokenize_fn(&tokenize_op, Token::Op("*"), "*");
        test_tokenize_fn(&tokenize_op, Token::Op("/"), "/");
        test_tokenize_fn(&tokenize_op, Token::Op("%"), "%");
        test_tokenize_fn(&tokenize_op, Token::Op("<"), "<");
        test_tokenize_fn(&tokenize_op, Token::Op(">"), ">");
        test_tokenize_fn(&tokenize_op, Token::Op("<="), "<=");
        test_tokenize_fn(&tokenize_op, Token::Op(">="), ">=");
        test_tokenize_fn(&tokenize_op, Token::Op("=="), "==");
    }

    #[test]
    fn test_tokenize_keyword() {
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("fn"), "fn");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("return"), "return");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("if"), "if");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("else"), "else");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("void"), "void");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("float"), "float");
    }

    #[test]
    fn test_tokenize_special_variable() {
        test_tokenize_fn(&tokenize_special_variable, Token::Special("now"), "now");
        test_tokenize_fn(&tokenize_special_variable, Token::Special("self"), "self");
    }

    #[test]
    fn test_tokenize_string() {
        test_tokenize_fn(
            &tokenize_string,
            Token::String("moji".to_string()),
            "\"moji\"",
        );
    }

    fn test_tokenize_1(expected: Vec<Token>, input: &str) {
        if let Ok(("", result)) = tokenize(input) {
            assert_eq!(expected, result);
        } else {
            println!("result = {:?}", tokenize(input));
            assert!(false);
        }
    }

    #[test]
    fn test_tokenize() {
        test_tokenize_1(vec![Token::Op("-"), Token::Float(127.0)], "-127");
        test_tokenize_1(
            vec![Token::Op("-"), Token::Float(127.0), Token::Float(127.0)],
            "-127 127",
        );
        test_tokenize_1(
            vec![
                Token::Op("-"),
                Token::Float(127.0),
                Token::Float(127.0),
                Token::Special("now"),
                Token::Float(0.0),
            ],
            "-127 127 now 0",
        );

        test_tokenize_1(
            vec![
                Token::Float(0.5),
                Token::Op("*"),
                Token::OpenParen,
                Token::Float(1.0),
                Token::Op("+"),
                Token::Float(2.0),
                Token::CloseParen,
            ],
            "0.5*(1+2)",
        );

        test_tokenize_1(
            vec![
                Token::String("abc".to_string()),
                Token::Op("+"),
                Token::String("123".to_string()),
            ],
            "\"abc\" + \"123\"",
        );

        test_tokenize_1(
            vec![
                Token::Keyword("fn"),
                Token::Identifier("func".to_string()),
                Token::OpenParen,
                Token::CloseParen,
                Token::FnReturnType,
                Token::Keyword("void"),
                Token::OpenBrace,
                Token::Keyword("return"),
                Token::Identifier("array".to_string()),
                Token::OpenBracket,
                Token::Float(0.0),
                Token::CloseBracket,
                Token::CloseBrace,
            ],
            "fn func() -> void { return array[0]}",
        );
    }
}

#[cfg(test)]
mod test_parse {
    use super::*;

    fn test_parse_1(expected: AST, string: &str) {
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
    }
}
