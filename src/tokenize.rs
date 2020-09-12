use std::string::ToString;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{
    char, digit1, line_ending, none_of, not_line_ending, one_of, space0, space1,
};
use nom::combinator::{all_consuming, map, opt, peek, rest, verify};
use nom::error::ErrorKind;
use nom::multi::{many0, many1};
use nom::sequence::tuple;
use nom::{Err, IResult};

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Fn,
    Return,
    If,
    Else,
    Float,
    Void,
}

impl Keyword {
    fn from_str(name: &str) -> Option<Keyword> {
        match name {
            "fn" => Some(Keyword::Fn),
            "return" => Some(Keyword::Return),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "float" => Some(Keyword::Float),
            "void" => Some(Keyword::Void),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Special {
    Now,
    SelfVar,
}

impl Special {
    fn from_str(name: &str) -> Option<Special> {
        match name {
            "now" => Some(Special::Now),
            "self" => Some(Special::SelfVar),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Float(f64),
    Keyword(Box<Keyword>),
    Special(Box<Special>),
    Op(&'a str),
    Identifier(String),
    String(String),
    LineComment(String),
    Comma,
    Colon,
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

pub fn token_type_eq<'a>(t1: &Token<'a>, t2: &Token<'a>) -> bool {
    match t1 {
        Token::Float(_) => match t2 {
            Token::Float(_) => true,
            _ => false,
        },
        Token::Keyword(_) => match t2 {
            Token::Keyword(_) => true,
            _ => false,
        },
        Token::Special(_) => match t2 {
            Token::Special(_) => true,
            _ => false,
        },
        Token::Op(_) => match t2 {
            Token::Op(_) => true,
            _ => false,
        },
        Token::Identifier(_) => match t2 {
            Token::Identifier(_) => true,
            _ => false,
        },
        Token::String(_) => match t2 {
            Token::String(_) => true,
            _ => false,
        },
        Token::LineComment(_) => match t2 {
            Token::LineComment(_) => true,
            _ => false,
        },
        Token::Comma => match t2 {
            Token::Comma => true,
            _ => false,
        },
        Token::Colon => match t2 {
            Token::Colon => true,
            _ => false,
        },
        Token::OpenParen => match t2 {
            Token::OpenParen => true,
            _ => false,
        },
        Token::CloseParen => match t2 {
            Token::CloseParen => true,
            _ => false,
        },
        Token::OpenBracket => match t2 {
            Token::OpenBracket => true,
            _ => false,
        },
        Token::CloseBracket => match t2 {
            Token::CloseBracket => true,
            _ => false,
        },
        Token::OpenBrace => match t2 {
            Token::OpenBrace => true,
            _ => false,
        },
        Token::CloseBrace => match t2 {
            Token::CloseBrace => true,
            _ => false,
        },
        Token::FnReturnType => match t2 {
            Token::FnReturnType => true,
            _ => false,
        },
        Token::Assign => match t2 {
            Token::Assign => true,
            _ => false,
        },
        Token::TimeAt => match t2 {
            Token::TimeAt => true,
            _ => false,
        },
        Token::Newline => match t2 {
            Token::Newline => true,
            _ => false,
        },
    }
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
    let (s, (_, frac)) = tuple((char('.'), digit1))(s)?;
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
    let (s, (name, _)) = tuple((
        alt((
            tag("fn"),
            tag("return"),
            tag("if"),
            tag("else"),
            tag("float"),
            tag("void"),
        )),
        alt((
            space1,
            all_consuming(space0),
            peek(map(one_of("{}()[]"), |_: char| "")),
        )),
    ))(s)?;
    if let Some(kw) = Keyword::from_str(name) {
        Ok((s, Token::Keyword(Box::new(kw))))
    } else {
        // TODO: it should be original error
        Err(Err::Error((s, ErrorKind::IsNot)))
    }
}

fn tokenize_special_variable(s: &str) -> IResult<&str, Token> {
    let (s, (name, _)) = tuple((
        alt((tag("now"), tag("self"))),
        alt((
            space1,
            peek(map(one_of("{}()[]"), |_: char| "")),
            all_consuming(space0),
        )),
    ))(s)?;
    if let Some(sp) = Special::from_str(name) {
        Ok((s, Token::Special(Box::new(sp))))
    } else {
        // TODO: it should be original error
        Err(Err::Error((s, ErrorKind::IsNot)))
    }
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

fn tokenize_delimiter(s: &str) -> IResult<&str, Token> {
    match one_of::<&str, &str, (&str, ErrorKind)>(":,")(s) {
        Ok((s, ',')) => Ok((s, Token::Comma)),
        Ok((s, ':')) => Ok((s, Token::Colon)),
        Ok((s, _)) => Err(Err::Error((s, ErrorKind::OneOf))),
        Err(err) => Err(err),
    }
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

    let (s, (first, rest)) = tuple((
        many1(one_of(&first_chars[..])),
        many0(one_of(&rest_chars[..])),
    ))(s)?;
    let first: String = first.into_iter().collect();
    let rest: String = rest.into_iter().collect();
    Ok((s, Token::Identifier([first, rest].concat())))
}

fn tokenize_newline(s: &str) -> IResult<&str, Token> {
    let (s, _) = line_ending(s)?;
    Ok((s, Token::Newline))
}

fn tokenize_line_comment(s: &str) -> IResult<&str, Token> {
    let (s, (_, comment, _)) = tuple((
        tag("//"),
        not_line_ending,
        alt((
            line_ending,
            verify(peek(rest), |rest: &str| rest.len() == 0),
        )),
    ))(s)?;
    Ok((s, Token::LineComment(comment.to_string())))
}

pub fn tokenize(s: &str) -> IResult<&str, Vec<Token>> {
    let mut tokens = Vec::new();
    let mut input = s;

    loop {
        let (s, _) = opt(space0::<&str, (&str, ErrorKind)>)(input)?;
        input = s;

        if input == "" {
            break;
        }

        let (s, token) = alt((
            tokenize_line_comment,
            tokenize_float,
            tokenize_fn_return_type,
            tokenize_op,
            tokenize_special_variable,
            tokenize_keyword,
            tokenize_delimiter,
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
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::Fn)),
            "fn",
        );
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::Return)),
            "return",
        );
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::If)),
            "if",
        );
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::Else)),
            "else",
        );
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::Void)),
            "void",
        );
        test_tokenize_fn(
            &tokenize_keyword,
            Token::Keyword(Box::new(Keyword::Float)),
            "float",
        );
    }

    #[test]
    fn test_tokenize_keyword_including_identifier() {
        if let Ok(("1", Token::Keyword(_))) = tokenize_keyword("fn1") {
            assert!(false)
        } else {
            assert!(true)
        }
    }

    #[test]
    fn test_tokenize_special_variable() {
        test_tokenize_fn(
            &tokenize_special_variable,
            Token::Special(Box::new(Special::Now)),
            "now",
        );
        test_tokenize_fn(
            &tokenize_special_variable,
            Token::Special(Box::new(Special::SelfVar)),
            "self",
        );

        test_tokenize_fn_with_error(&tokenize_keyword, "self_hoge");
    }

    #[test]
    fn test_tokenize_line_comment() {
        test_tokenize_fn(
            &tokenize_line_comment,
            Token::LineComment("abcd".to_string()),
            "//abcd\n",
        );

        test_tokenize_fn(
            &tokenize_line_comment,
            Token::LineComment(" abcd".to_string()),
            "// abcd\n",
        );

        test_tokenize_fn(
            &tokenize_line_comment,
            Token::LineComment(" abcd".to_string()),
            "// abcd",
        );

        test_tokenize_fn_with_error(&tokenize_line_comment, "self_hoge");
    }

    #[test]
    fn test_tokenize_special_variables_including_identifier() {
        if let Ok(("now", Token::Keyword(_kw))) = tokenize_keyword("nownow") {
            assert!(false)
        } else {
            assert!(true)
        }
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
                Token::Special(Box::new(Special::Now)),
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
                Token::Keyword(Box::new(Keyword::Fn)),
                Token::Identifier("func".to_string()),
                Token::OpenParen,
                Token::CloseParen,
                Token::FnReturnType,
                Token::Keyword(Box::new(Keyword::Void)),
                Token::OpenBrace,
                Token::Keyword(Box::new(Keyword::Return)),
                Token::Identifier("array".to_string()),
                Token::OpenBracket,
                Token::Float(0.0),
                Token::CloseBracket,
                Token::CloseBrace,
            ],
            "fn func() -> void { return array[0]}",
        );
    }

    #[test]
    fn test_tokenize_with_comment() {
        test_tokenize_1(
            vec![
                Token::LineComment("abc".to_string()),
                Token::Identifier("a".to_string()),
                Token::Op("+"),
                Token::Identifier("b".to_string()),
                Token::Newline,
                Token::LineComment("fff".to_string()),
            ],
            "//abc\na+b\n//fff",
        )
    }
}
