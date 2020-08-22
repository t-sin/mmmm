extern crate nom;

use nom::branch::permutation;
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::{all_consuming, opt};
use nom::error::ErrorKind;
use nom::{Err, IResult};

#[derive(Debug)]
enum AST {
    Float(f64),
}

fn parse_int(s: &str) -> IResult<&str, f64> {
    let (s, int) = digit1(s)?;
    if let Ok(int) = int.parse::<f64>() {
        Ok((s, int))
    } else {
        Err(Err::Failure((s, ErrorKind::Digit)))
    }
}

fn parse_fract(s: &str) -> IResult<&str, f64> {
    let (s, (_, frac)) = permutation((char('.'), digit1))(s)?;
    if let Ok(frac) = frac.parse::<f64>() {
        Ok((s, frac))
    } else {
        Err(Err::Failure((s, ErrorKind::Digit)))
    }
}

fn parse_float(s: &str) -> IResult<&str, AST> {
    let (s, sign) = opt(one_of("+-"))(s)?;
    let sign: f64 = if sign.unwrap_or('+') == '+' {
        1.0
    } else {
        -1.0
    };

    let (s, int) = parse_int(s)?;
    match opt(parse_fract)(s) {
        Ok((s, Some(frac))) => {
            let float = (int + frac) * sign;
            Ok((s, AST::Float(float)))
        }
        Ok((s, None)) => {
            let float = int * sign;
            Ok((s, AST::Float(float)))
        }
        err => Err(Err::Failure((s, ErrorKind::Float))),
    }
}

fn parse(s: &str) -> IResult<&str, AST> {
    all_consuming(parse_float)(s)
}

fn main() {
    let input = "-123.0";
    if let Ok((s, n)) = parse(input) {
        println!("{:?}, {:?}", s, n);
    } else {
        panic!("parse error!");
    }
}
