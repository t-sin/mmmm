extern crate nom;

use nom::branch::permutation;
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::{all_consuming, opt};
use nom::error::ErrorKind;
use nom::{Err, IResult};

#[derive(Debug, PartialEq)]
enum Token {
    Float(f64),
}

fn parse_int(s: &str) -> IResult<&str, f64> {
    match digit1::<&str, (&str, ErrorKind)>(s) {
        Ok((s, "")) => Err(Err::Failure((s, ErrorKind::Digit))),
        Ok((s, int)) => {
            if let Ok(int) = int.parse::<f64>() {
                Ok((s, int))
            } else {
                Err(Err::Failure((s, ErrorKind::Digit)))
            }
        }
        err => Err(Err::Failure((s, ErrorKind::Digit))),
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
        return Err(Err::Failure((s, ErrorKind::Eof)));
    }

    let (s, int) = match parse_int(s) {
        Ok((s, int)) => (s, int),
        err => return Err(Err::Failure((s, ErrorKind::Digit))),
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
        _err => Err(Err::Failure((s, ErrorKind::Float))),
    }
}

fn parse(s: &str) -> IResult<&str, Token> {
    all_consuming(parse_float)(s)
}

fn main() {
    let input = "-123.0";
    match parse(input) {
        Ok((_, n)) => println!("parsed: {:?}", n),
        err => panic!("parse error: {:?}", err),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type ParseFn = dyn Fn(&str) -> IResult<&str, Token>;

    fn test_parse_fn(parse_fn: &ParseFn, expected: Token, input: &str) {
        if let Ok(("", result)) = parse_fn(input) {
            assert_eq!(expected, result);
        } else {
            assert!(false);
        }
    }

    fn test_parse_fn_with_error(parse_fn: &ParseFn, input: &str) {
        if let Ok(("", result)) = parse_fn(input) {
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
}
