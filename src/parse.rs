use nom::branch::{alt, permutation};
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, multispace0, none_of, one_of};
use nom::combinator::opt;
use nom::error::ErrorKind;
use nom::multi::{many0, many1};
use nom::sequence::tuple;
use nom::{Err, IResult};

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Float(f64),
    Keyword(&'a str),
    BinaryOp(&'a str),
    Identifier(String),
    String(String),
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    FnReturnType,
    Assign,
    TimeAt,
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
    let (s, sign) = opt(char('-'))(s)?;
    let sign: f64 = if let None = sign { 1.0 } else { -1.0 };

    if s == "" {
        return Err(Err::Error((s, ErrorKind::Eof)));
    }

    let (s, int) = match tokenize_int(s) {
        Ok((s, int)) => (s, int),
        _ => return Err(Err::Error((s, ErrorKind::Digit))),
    };
    match opt(tokenize_fract)(s) {
        Ok((s, Some(frac))) => {
            let float = (int + frac).copysign(sign);
            Ok((s, Token::Float(float)))
        }
        Ok((s, None)) => {
            let float = int.copysign(sign);
            Ok((s, Token::Float(float)))
        }
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
        tag("now"),
    ))(s)?;
    Ok((s, Token::Keyword(name)))
}

fn tokenize_binop(s: &str) -> IResult<&str, Token> {
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
    ))(s)?;
    Ok((s, Token::BinaryOp(op)))
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
            tokenize_binop,
            tokenize_keyword,
            tokenize_parens,
            tokenize_string,
            tokenize_assignment,
            tokenize_time_at,
            tokenize_identifier,
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
        "<" | ">" | "<=" | ">=" | "==" => 5,
        "&&" => 6,
        "||" => 7,
        _ => -1, // そんな演算子はないエラー
    }
}

fn parse_expression<'a>(t: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], AST> {
    Ok((t, AST::Exp(Box::new(Exp::Float(0.0)))))
}

fn parse_1<'a>(t: &'a [Token]) -> IResult<&'a [Token<'a>], AST> {
    parse_float(t)
}

pub fn parse<'a>(t: &'a [Token]) -> IResult<&'a [Token<'a>], Vec<AST>> {
    let mut asts = Vec::new();
    let mut input = t;

    loop {
        if input == &[] {
            break;
        }

        let (t, ast) = parse_1(&input)?;
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
        test_tokenize_fn_with_error(&tokenize_float, "-.0");
        test_tokenize_fn_with_error(&tokenize_float, ".012");
    }

    #[test]
    fn test_tokenize_float() {
        test_tokenize_fn(&tokenize_float, Token::Float(0.0), "0");
        test_tokenize_fn(&tokenize_float, Token::Float(0.0), "0.0");
        test_tokenize_fn(&tokenize_float, Token::Float(0.0), "-0");

        test_tokenize_fn(&tokenize_float, Token::Float(1.0), "1");
        test_tokenize_fn(&tokenize_float, Token::Float(123.0), "123");

        test_tokenize_fn(&tokenize_float, Token::Float(123.0), "123.0");
        test_tokenize_fn(&tokenize_float, Token::Float(123.012), "123.012");

        test_tokenize_fn(&tokenize_float, Token::Float(-1.0), "-1");
        test_tokenize_fn(&tokenize_float, Token::Float(-1.0), "-1.0");
        test_tokenize_fn(&tokenize_float, Token::Float(-127.0), "-127");
    }

    #[test]
    fn test_tokenize_binop() {
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("+"), "+");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("-"), "-");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("*"), "*");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("/"), "/");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("%"), "%");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("<"), "<");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp(">"), ">");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("<="), "<=");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp(">="), ">=");
        test_tokenize_fn(&tokenize_binop, Token::BinaryOp("=="), "==");
    }

    #[test]
    fn test_tokenize_keyword() {
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("fn"), "fn");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("return"), "return");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("if"), "if");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("else"), "else");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("void"), "void");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("float"), "float");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("now"), "now");
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
        test_tokenize_1(vec![Token::Float(-127.0)], "-127");
        test_tokenize_1(vec![Token::Float(-127.0), Token::Float(127.0)], "-127 127");
        test_tokenize_1(
            vec![
                Token::Float(-127.0),
                Token::Float(127.0),
                Token::Keyword("now"),
                Token::Float(0.0),
            ],
            "-127 127 now 0",
        );

        test_tokenize_1(
            vec![
                Token::Float(0.5),
                Token::BinaryOp("*"),
                Token::OpenParen,
                Token::Float(1.0),
                Token::BinaryOp("+"),
                Token::Float(2.0),
                Token::CloseParen,
            ],
            "0.5*(1+2)",
        );

        test_tokenize_1(
            vec![
                Token::String("abc".to_string()),
                Token::BinaryOp("+"),
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

    #[test]
    fn test_parse() {
        if let Ok(("", tokens)) = tokenize("0.0") {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(1, vec.len());

                    if let Some(AST::Exp(exp)) = vec.iter().nth(0) {
                        if let Exp::Float(f) = **exp {
                            assert_eq!(0.0, f);
                        } else {
                            println!("error: {:?}", vec);
                            assert!(false);
                        }
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
}
