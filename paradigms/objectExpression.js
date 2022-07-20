"use strict";
const make_expr = (expr, evaluate, diff, toString, prefix, postfix) => {
    expr.prototype.evaluate = evaluate;
    expr.prototype.diff = diff;
    expr.prototype.toString = toString;
    expr.prototype.prefix = prefix !== undefined ? prefix : toString;
    expr.prototype.postfix = postfix !== undefined ? postfix : toString;
    return expr; }
const Const = make_expr(function (value) { this.value = value },
    function () { return this.value },
    function () { return Const.zero },
    function () { return this.value.toString() })
Const.e = new Const(Math.E);
Const.zero = new Const(0);
Const.one = new Const(1);
Const.two = new Const(2);

const variables = ['x', 'y', 'z'];
const Variable = make_expr(function (s) { this.s = s },
    function (...args) { return args[variables.indexOf(this.s)] },
    function (variable) { return this.s === variable ? Const.one : Const.zero },
    function () { return this.s })

let operations = {};
const operation = make_expr(function (...args) { this.args = args },
    function (...vars) { return this.f(...this.args.map(g => g.evaluate(...vars))) },
    function (xyz) { return this.d(...this.args)(...this.args.map(g => g.diff(xyz))) },
    function () { return this.args.map(g => g.toString()).join(' ') + ' ' + this.symbol },
    function () { return '(' + this.symbol + ' ' + this.args.map(g => g.prefix()).join(' ') + ')'},
    function () { return '(' + this.args.map(g => g.postfix()).join(' ') + ' ' + this.symbol + ')'})
const make_op = function (symbol, f, d) {
    const op = function (...args) { operation.call(this, ...args) }
    operations[symbol] =  op;
    op.prototype = Object.create(operation.prototype);
    op.prototype.symbol = symbol;
    op.prototype.f = f;
    op.prototype.d = d;
    return op; }
const Negate = make_op('negate', a => -a,
    a => ai => new Negate(ai))
const Add = make_op('+', (a, b) => a + b,
    (a, b) => (ai, bi) => new Add(ai, bi))
const Subtract = make_op('-', (a, b) => a - b,
    (a, b) => (ai, bi) => new Subtract(ai, bi))
const Multiply = make_op('*', (a, b) => a * b,
    (a, b) => (ai, bi) => new Add(new Multiply(ai, b), new Multiply(a, bi)))
const Divide = make_op('/', (a, b) => a / b,
    (a, b) => (ai, bi) => new Divide(new Subtract(new Multiply(ai, b), new Multiply(a, bi)), new Multiply(b, b)))
const Pow = make_op('pow', (a, b) => a ** b,
    (a, b) => (ai, bi) => new Multiply(new Pow(a, b), new Add(new Multiply(bi, new Log(Const.e, a)),
        new Divide(new Multiply(b, ai), a))))
const Log = make_op('log', (a, b) => Math.log(Math.abs(b)) / Math.log(Math.abs(a)),
    (a, b) => (ai, bi) => new Divide(new Subtract(new Divide(new Multiply(new Log(Const.e, a), bi), b),
        new Divide(new Multiply(ai, new Log(Const.e, b)), a)), new Pow(new Log(Const.e, a), Const.two)))
const Mean = make_op('mean', (...w) => w.length === 0 ? 0 : [...w].reduce((wa, wb) => wa + wb, 0) / w.length,
    (...w) => (...wi) => w.length === 0 ? Const.zero :
        new Divide([...wi].reduce((wai, wbi) => new Add(wai, wbi), Const.zero), new Const(w.length)))
const Var = make_op('var', (...w) => w.length === 0 ? 0 : [...w].reduce((wa, wb) => wa + (wb ** 2), 0)
        / w.length - ([...w].reduce((wa, wb) => wa + wb, 0) / w.length) ** 2,
    (...w) => (...wi) => w.length === 0 ? Const.zero :
        new Multiply(new Subtract([...w.map((x, i) => [x, wi[i]])].reduce((wa, wb) => new Add(wa,
        new Multiply(...wb)), Const.zero), new Multiply(new Mean(...w), [...wi].reduce((wai, wbi) => new Add(wai, wbi),
        Const.zero))), new Const(2 / w.length)))

const parse_token = (stack, t) => {
    if (t in operations) stack.push(new operations[t](...stack.splice(-operations[t].prototype.f.length)));
    else if (variables.includes(t)) stack.push(new Variable(t));
    else stack.push(new Const(parseInt(t)));
    return stack;
}
const parse_with_modifier = (expression, is_prefix_mode) => {
    if (expression.length === 0) throw new empty_input_error().toString();
    expression = expression.split(/\s+|([()])/).filter(x => x !== undefined).map(x => x.trim())
        .filter(x => x.length > 0);
    let pos = 0;
    const current = _ => expression[pos];
    const take = _ => expression[pos++];

    const is_number = s => '0' <= s[0] <= '9' || s[0] === '-';
    const next_number = _ => {
        const z = parseInt(current())
        if (z.toString().length !== current().length) throw new missing_whitespace_error(current()).toString();
        take();
        return z;
    }
    const parse_argument = _ => {
        if (current() === '(') return parse_in_brackets(take());
        if (variables.includes(current())) return new Variable(take());
        if (is_number(current())) return new Const(next_number());
        throw new unexpected_token_error(current()).toString();
    }
    const parse_in_brackets = _ => {
        let stack = [], op;
        if (is_prefix_mode) op = take();
        while ((is_prefix_mode ? current() !== ')' : (!(current() in operations))) && pos < expression.length)
            stack.push(parse_argument());
        if (!is_prefix_mode) op = take();
        if (!(op in operations)) {
            is_prefix_mode ? stack.unshift(op) : stack.push(op)
            throw new missing_operation_error(...stack).toString();
        }
        if (take() !== ')') throw new missing_bracket_error(')', op).toString();
        const operation = operations[op];
        const arity = operation.prototype.f.length;
        if (arity !== stack.length && arity !== 0)
            throw new invalid_amount_of_args_error(op, arity, stack.length).toString();
        return new operation(...stack);
    }
    const res = current() === '(' ? parse_in_brackets(take()) : parse_argument();
    if (pos !== expression.length) throw new unexpected_continuation_error(...expression.slice(pos)).toString();
    return res;
}
const make_error = (name, message) => {
    const parsing_error = function(...args) {
        this.name = name;
        this.message = message(...args);
    }
    parsing_error.prototype = Object.create(Error.prototype);
    parsing_error.prototype.constructor = parsing_error;
    return parsing_error;
}
const empty_input_error = make_error('empty input error', _ => 'empty input :(')
const missing_bracket_error = make_error('missing bracket error',
    (bracket, op) => 'missing \'' + bracket + '\' for \'' + op + '\'')
const unexpected_token_error = make_error('unexpected token error',
    (token) => 'can\'t parse token \'' + token + '\'')
const missing_whitespace_error = make_error('missing whitespace error',
    (token) => 'whitespace missing between tokens at \'' + token + '\'')
const invalid_amount_of_args_error = make_error('invalid amount of args error',
    (op, expected, found) => 'expected ' + expected + ' arguments for ' + op + ', found ' + found)
const missing_operation_error = make_error('missing operation error',
    (...tokens) => 'expected operation at \'' + tokens.join(' ') + '\'')
const unexpected_continuation_error = make_error('unexpected continuation of expr error',
    (...tokens) => 'expected end of expression, found: \'' + tokens.join(' ') + '\'')

const parse = expression => expression.trim().split(/\s+/).reduce(parse_token, []).pop();
const parsePrefix = expression => parse_with_modifier(expression, true);
const parsePostfix = expression => parse_with_modifier(expression, false);