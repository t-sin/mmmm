extern crate clap;
extern crate nom;

mod parse;
mod tokenize;

use std::fs::File;
use std::io::prelude::*;
use std::io::{self};

use clap::{App, Arg};

fn main() {
    let matches = App::new("mmmm")
        .version("0.1.0")
        .author("t-sin <shinichi.tanaka45@gmail.com>")
        .arg(
            Arg::with_name("filename")
                .help("Input file name")
                .required(false),
        )
        .get_matches();
    let mut input = String::new();
    match matches.value_of("filename") {
        Some("-") => {
            let _ = io::stdin().read_to_string(&mut input);
        }
        Some(filename) => {
            let _ = File::open(filename).unwrap().read_to_string(&mut input);
        }
        None => {
            let _ = io::stdin().read_to_string(&mut input);
        }
    };
    let input = &input;

    let tokens = match tokenize::tokenize(input) {
        Ok((_, tokens)) => Some(tokens),
        err => panic!("tokenize error: {:?}", err),
    };

    let asts = match parse::parse(&tokens.unwrap()) {
        Ok((_, asts)) => Some(asts),
        err => panic!("parse error: {:?}", err),
    };

    println!("AST = {:?}", asts);
}
