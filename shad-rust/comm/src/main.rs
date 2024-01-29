#![forbid(unsafe_code)]
use std::{
    collections::HashSet,
    env::args,
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let args = args().collect::<Vec<String>>();
    assert!(args.len() == 3);
    let mut lines1: HashSet<String> = HashSet::new();
    for line in BufReader::new(File::open(args[1].clone()).unwrap()).lines() {
        if line.is_ok() {
            lines1.insert(line.unwrap());
        }
    }
    for line in BufReader::new(File::open(args[2].clone()).unwrap()).lines() {
        if line.is_ok() {
            let taken = lines1.take(&line.unwrap());
            if taken != None {
                println!("{}", taken.unwrap());
            }
        }
    }
}
