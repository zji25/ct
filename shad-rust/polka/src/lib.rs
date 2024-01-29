#![forbid(unsafe_code)]

use std::{collections::HashMap, fmt::Display};

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Symbol(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Symbol(sym) => write!(f, "'{}", sym),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct Interpreter {
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            variables: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: &str) {
        let tokens = expr.split_whitespace().filter(|x| !x.is_empty());
        for token in tokens {
            eprintln!("{}", token);
            match token {
                "set" => {
                    let key = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    if let Value::Symbol(symbol) = key {
                        self.variables.insert(symbol, value);
                    } else {
                        panic!("set was called on wrong operators");
                    }
                }
                _ if token.starts_with('\'') => {
                    self.stack.push(Value::Symbol(token[1..].to_string()));
                }
                _ if token.starts_with('$') => {
                    self.stack
                        .push(self.variables.get(&token[1..].to_string()).unwrap().clone());
                }
                "+" | "-" | "*" | "/" => {
                    if let (Value::Number(a), Value::Number(b)) =
                        (self.stack.pop().unwrap(), self.stack.pop().unwrap())
                    {
                        let op = match token {
                            "+" => a + b,
                            "-" => a - b,
                            "*" => a * b,
                            "/" => a / b,
                            _ => panic!("shouldn't come here"),
                        };
                        self.stack.push(Value::Number(op));
                    } else {
                        panic!("{} was called on wrong operators", token);
                    }
                }
                _ => match token.parse::<f64>() {
                    Ok(n) => {
                        self.stack.push(Value::Number(n));
                    }
                    _ => {
                        panic!("shouldn't come here");
                    }
                },
            }
        }
    }

    pub fn stack(&self) -> &[Value] {
        &self.stack
    }
}
