extern crate clap;
extern crate nom;

mod parse;

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

    match parse::tokenize(input) {
        Ok((_, tokens)) => {
            println!("parsed: {:?}", tokens);
            let ast = parse::parse(&tokens);
            println!("ast: {:?}", ast);
        }
        err => panic!("parse error: {:?}", err),
    }
}
