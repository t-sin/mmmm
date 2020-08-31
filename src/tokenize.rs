use nom::branch::{alt, permutation};
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{
    char, digit1, multispace0, newline, none_of, one_of, space0, space1,
};
use nom::combinator::{all_consuming, opt};
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
    LineComment(String),
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
    let (s, (name, _)) = permutation((
        alt((
            tag("fn"),
            tag("return"),
            tag("if"),
            tag("else"),
            tag("float"),
            tag("void"),
        )),
        alt((space1, all_consuming(space0))),
    ))(s)?;
    Ok((s, Token::Keyword(name)))
}

fn tokenize_special_variable(s: &str) -> IResult<&str, Token> {
    let (s, (name, _)) = permutation((
        alt((tag("now"), tag("self"))),
        alt((space1, all_consuming(space0))),
    ))(s)?;
    Ok((s, Token::Special(name)))
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

fn tokenize_line_comment(s: &str) -> IResult<&str, Token> {
    let (s, (_, comment, _)) = permutation((tag("//"), many0(is_not("\n")), opt(newline)))(s)?;
    let comment: String = comment.into_iter().collect();
    Ok((s, Token::LineComment(comment)))
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
            tokenize_line_comment,
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
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("fn"), "fn");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("return"), "return");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("if"), "if");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("else"), "else");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("void"), "void");
        test_tokenize_fn(&tokenize_keyword, Token::Keyword("float"), "float");
    }
    #[test]
    fn test_tokenize_keyword_including_identifier() {
        if let Ok(("1", Token::Keyword("fn"))) = tokenize_keyword("fn1") {
            assert!(false)
        } else {
            assert!(true)
        }
    }

    #[test]
    fn test_tokenize_special_variable() {
        test_tokenize_fn(&tokenize_special_variable, Token::Special("now"), "now");
        test_tokenize_fn(&tokenize_special_variable, Token::Special("self"), "self");

        test_tokenize_fn_with_error(&tokenize_keyword, "self_hoge");
    }

    #[test]
    fn test_tokenize_special_variables_including_identifier() {
        if let Ok(("now", Token::Keyword("now"))) = tokenize_keyword("nownow") {
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
