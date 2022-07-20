"use strict";
const cnst = value => _ => value;
const variables = ['x', 'y', 'z'];
const variable = s => (...args) => args[variables.indexOf(s)];
function apply (f) {
    const fun = (...args) => (...vars) => f(...(args.map(g => g(...vars))));
    fun.arity = f.length;
    return fun;
}
const negate = apply(a => -a);
const abs = apply(a => a > 0 ? a : -a);
const add = apply((a, b) => a + b);
const subtract = apply((a, b) => a - b);
const multiply = apply((a, b) => a * b);
const divide = apply((a, b) => a / b);
const iff = apply((a, b, c) => a >= 0 ? b : c);
const operations = {
    'negate': negate, 'abs': abs,
    '+': add, '-': subtract, '*': multiply, '/': divide,
    'iff': iff
};
const pi = cnst(Math.PI);
const e = cnst(Math.E);
const constant_values = {'pi': pi, 'e': e};

const parse_token = (stack, t) => {
    if (t in operations) stack.push(operations[t](...stack.splice(-operations[t].arity)));
    else if (variables.includes(t)) stack.push(variable(t));
    else if (t in constant_values) stack.push(constant_values[t])
    else stack.push(cnst(parseInt(t)))
    return stack;
}
const parse = expression => expression.trim().split(/\s+/).reduce(parse_token, []).pop();

for (let i = 0; i < 11; i++) console.log(parse('x x * x 2 * - 1 +')(i));
console.log(parse('723 -477 negate -')(0))