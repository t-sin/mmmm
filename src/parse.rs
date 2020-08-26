use nom::branch::{alt, permutation};
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, multispace0, none_of, one_of};
use nom::combinator::{all_consuming, opt};
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

fn parse_int(s: &str) -> IResult<&str, f64> {
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

fn parse_fract(s: &str) -> IResult<&str, f64> {
    let (s, (_, frac)) = permutation((char('.'), digit1))(s)?;
    let frac = frac.parse::<f64>().unwrap() / 10f64.powf(frac.len() as f64);
    Ok((s, frac))
}

fn parse_float(s: &str) -> IResult<&str, Token> {
    let (s, sign) = opt(char('-'))(s)?;
    let sign: f64 = if let None = sign { 1.0 } else { -1.0 };

    if s == "" {
        return Err(Err::Error((s, ErrorKind::Eof)));
    }

    let (s, int) = match parse_int(s) {
        Ok((s, int)) => (s, int),
        _ => return Err(Err::Error((s, ErrorKind::Digit))),
    };
    match opt(parse_fract)(s) {
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

fn parse_keyword(s: &str) -> IResult<&str, Token> {
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

fn parse_binop(s: &str) -> IResult<&str, Token> {
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

fn parse_assignment(s: &str) -> IResult<&str, Token> {
    let (s, _) = char('=')(s)?;
    Ok((s, Token::Assign))
}

fn parse_time_at(s: &str) -> IResult<&str, Token> {
    let (s, _) = char('@')(s)?;
    Ok((s, Token::TimeAt))
}

fn parse_fn_return_type(s: &str) -> IResult<&str, Token> {
    let (s, _) = tag("->")(s)?;
    Ok((s, Token::FnReturnType))
}

fn parse_string(s: &str) -> IResult<&str, Token> {
    let (s, (_, string, _)) = tuple((char('"'), many1(none_of("\"")), char('"')))(s)?;
    let string: String = string.iter().collect();
    Ok((s, Token::String(string)))
}

fn parse_parens(s: &str) -> IResult<&str, Token> {
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

fn parse_identifier(s: &str) -> IResult<&str, Token> {
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
            parse_float,
            parse_fn_return_type,
            parse_binop,
            parse_keyword,
            parse_parens,
            parse_string,
            parse_assignment,
            parse_time_at,
            parse_identifier,
        ))(input)?;
        input = s;
        tokens.push(token);
    }

    Ok((input, tokens))
}

pub enum AST {
    Float(f64),
}

pub fn parse<'a>(_s: &'a [Token]) -> IResult<&'a [Token<'a>], AST> {
    Ok((&[], AST::Float(0.0)))
}

#[cfg(test)]
mod test {
    use super::*;

    type ParseFn = dyn Fn(&str) -> IResult<&str, Token>;

    fn test_parse_fn(parse_fn: &ParseFn, expected: Token, input: &str) {
        if let Ok(("", result)) = parse_fn(input) {
            assert_eq!(expected, result);
        } else {
            println!("result = {:?}", parse_fn(input));
            assert!(false);
        }
    }

    fn test_parse_fn_with_error(parse_fn: &ParseFn, input: &str) {
        if let Ok(("", _)) = parse_fn(input) {
            println!("result = {:?}", parse_fn(input));
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_parse_float_for_empty() {
        test_parse_fn_with_error(&parse_float, "");
    }

    #[test]
    fn test_invalid_float() {
        test_parse_fn_with_error(&parse_float, ".0");
        test_parse_fn_with_error(&parse_float, "-.0");
        test_parse_fn_with_error(&parse_float, ".012");
    }

    #[test]
    fn test_parse_float() {
        test_parse_fn(&parse_float, Token::Float(0.0), "0");
        test_parse_fn(&parse_float, Token::Float(0.0), "0.0");
        test_parse_fn(&parse_float, Token::Float(0.0), "-0");

        test_parse_fn(&parse_float, Token::Float(1.0), "1");
        test_parse_fn(&parse_float, Token::Float(123.0), "123");

        test_parse_fn(&parse_float, Token::Float(123.0), "123.0");
        test_parse_fn(&parse_float, Token::Float(123.012), "123.012");

        test_parse_fn(&parse_float, Token::Float(-1.0), "-1");
        test_parse_fn(&parse_float, Token::Float(-1.0), "-1.0");
        test_parse_fn(&parse_float, Token::Float(-127.0), "-127");
    }

    #[test]
    fn test_parse_binop() {
        test_parse_fn(&parse_binop, Token::BinaryOp("+"), "+");
        test_parse_fn(&parse_binop, Token::BinaryOp("-"), "-");
        test_parse_fn(&parse_binop, Token::BinaryOp("*"), "*");
        test_parse_fn(&parse_binop, Token::BinaryOp("/"), "/");
        test_parse_fn(&parse_binop, Token::BinaryOp("%"), "%");
        test_parse_fn(&parse_binop, Token::BinaryOp("<"), "<");
        test_parse_fn(&parse_binop, Token::BinaryOp(">"), ">");
        test_parse_fn(&parse_binop, Token::BinaryOp("<="), "<=");
        test_parse_fn(&parse_binop, Token::BinaryOp(">="), ">=");
        test_parse_fn(&parse_binop, Token::BinaryOp("=="), "==");
    }

    #[test]
    fn test_parse_keyword() {
        test_parse_fn(&parse_keyword, Token::Keyword("fn"), "fn");
        test_parse_fn(&parse_keyword, Token::Keyword("return"), "return");
        test_parse_fn(&parse_keyword, Token::Keyword("if"), "if");
        test_parse_fn(&parse_keyword, Token::Keyword("else"), "else");
        test_parse_fn(&parse_keyword, Token::Keyword("void"), "void");
        test_parse_fn(&parse_keyword, Token::Keyword("float"), "float");
        test_parse_fn(&parse_keyword, Token::Keyword("now"), "now");
    }

    #[test]
    fn test_parse_string() {
        test_parse_fn(&parse_string, Token::String("moji".to_string()), "\"moji\"");
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
