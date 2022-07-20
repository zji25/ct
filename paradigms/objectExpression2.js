"use strict";
const make_expr = (expr, evaluate, diff, simplify, toString, prefix, postfix) => {
    expr.prototype.evaluate = evaluate;
    expr.prototype.diff = diff;
    expr.prototype.simplify = simplify;
    expr.prototype.toString = toString;
    expr.prototype.prefix = prefix !== undefined ? prefix : toString;
    expr.prototype.postfix = postfix !== undefined ? postfix : toString;
    return expr; }
const Const = make_expr(function (value) { this.value = value },
    function () { return this.value },
    function () { return Const.zero },
    function () { return this },
    function () { return this.value.toString() })
Const.zero = new Const(0);
Const.one = new Const(1);
Const.two = new Const(2);

const variables = ['x', 'y', 'z'];
const Variable = make_expr(function (s) { this.s = s },
    function (...args) { return args[variables.indexOf(this.s)] },
    function (variable) { return this.s === variable ? Const.one : Const.zero },
    function () { return this },
    function () { return this.s })

const operation = make_expr(function (...args) { this.args = args },
    function (...vars) { return this.f(...this.args.map(g => g.evaluate(...vars))) },
    function (xyz) { return this.d(...this.args)(...this.args.map(g => g.diff(xyz))) },
    function() { const sd = this.args.map(x => x.simplify());
        if (sd.every(x => x instanceof Const)) return new Const(this.f(...sd.map(x => x.value)));
        return this.s(...sd) },
    function () { return this.args.map(g => g.toString()).join(' ') + ' ' + this.symbol },
    function () { return '(' + this.symbol + ' ' + this.args.map(g => g.prefix()).join(' ') + ')'},
    function () { return '(' + this.args.map(g => g.postfix()).join(' ') + ' ' + this.symbol + ')'})
let operations = {};
const make_op = function (symbol, f, d, s) {
    const op = function (...args) { operation.call(this, ...args) }
    operations[symbol] = op;
    op.prototype = Object.create(operation.prototype);
    op.prototype.symbol = symbol;
    op.prototype.f = f;
    op.prototype.d = d;
    op.prototype.s = s;
    return op; }

const ch = (a, c) => a instanceof Const && (c === undefined || a.value === c);
const eq = (a, b) => typeof a === 'object' && typeof b === 'object' ? Object.keys(a).length === Object.keys(b).length
    && Object.keys(a).reduce((is_eq, k) => is_eq && eq(a[k], b[k]), true) : a === b;
const rec = (a, search) => { if (!(a instanceof Multiply)) return;
    let z = a.args.find(x => eq(x, search)), flag = false;
    if (z) return a.args.some(x => !eq(x, search)) ? a.args.find(x => !eq(x, search)) : search;
    z = [...a.args].map(x => {
        const ww = rec(x, search);
        if (ww && !flag) { flag = true; return ww; }
        return x; });
    if (!flag) z = [...a.args].map(x => {
        if (x instanceof Pow && eq(x.args[0], search) && !flag) {
            flag = true; return new Pow(x.args[0], new Subtract(x.args[1], Const.one)); }
        return x; })
    if (flag) return new Multiply(...z); }
const neg_impl = a => {
    if (a instanceof Negate) return a.args[0];
    if (a instanceof Subtract) return new Subtract(a.args[1], a.args[0]);
    if (a instanceof Multiply && a.args.some(x => ch(x, -1))) return a.args.find(x => !ch(x, -1));
    if ((a instanceof Multiply || a instanceof Divide) && a.args.some(x => ch(x))) {
        let z = [...a.args], i = z.findIndex(x => ch(x));
        z[i] = new Const(-z[i].value); return a.s(...z); }}
const div_impl = (a, b) => { let r, wa, wb;
    if ((r = rec(a, b))) return r;
    if ((r = rec(b, a))) return eq(r, b) ? r : new Divide(Const.one, r);
    if ((wa = neg_impl(a)) && (wb = neg_impl(b))) return new Divide(wa, wb);
    if (a instanceof Divide)
        if (ch(a.args[0]) && ch(b)) return new Divide(new Const(a.args[0].value / b.value), a.args[1]);
        else return new Divide(a.args[0], new Multiply(a.args[1], b)); }
let z;
const Negate = make_op('negate', a => -a, a => ai => new Negate(ai), a => ch(a, 0) ? Const.zero :
    a instanceof Const ? new Const(-a.value) : (z = neg_impl(a)) ? z.simplify() : new Negate(a))
const Ln = make_op('ln', a => Math.log(Math.abs(a)), a => ai => new Divide(ai, a), a => new Ln(a))
const Add = make_op('+', (a, b) => a + b, (a, b) => (ai, bi) => new Add(ai, bi),
    (a, b) => ch(a, 0) ? b : ch(b, 0) ? a : new Add(a, b))
const Subtract = make_op('-', (a, b) => a - b, (a, b) => (ai, bi) => new Subtract(ai, bi),
    (a, b) => ch(a, 0) ? new Negate(b).simplify() : ch(b, 0) ? a : new Subtract(a, b))
const Multiply = make_op('*', (a, b) => a * b,
    (a, b) => (ai, bi) => new Add(new Multiply(ai, b), new Multiply(a, bi)),
    (a, b) => ch(a, 1) ? b : ch(b, 1) ? a : ch(a, -1) ? new Negate(b).simplify() : ch(b, -1) ?
        new Negate(a).simplify() : (ch(a, 0) || ch(b, 0)) ? Const.zero : new Multiply(a, b))
const Divide = make_op('/', (a, b) => a / b,
    (a, b) => (ai, bi) => new Divide(new Subtract(new Multiply(ai, b), new Multiply(a, bi)), new Multiply(b, b)),
    (a, b) => ch(a, 0) ? Const.zero : ch(b, 1) ? a : ch(b, -1) ? new Negate(a).simplify() : eq(a, b) ?
        Const.one : (z = div_impl(a, b)) ? z.simplify() : new Divide(a, b))
const Pow = make_op('pow', (a, b) => a ** b,
    (a, b) => (ai, bi) => ch(b) ? new Multiply(new Multiply(new Const(b.value), new Pow(a, new Const(b.value - 1))),
        ai) : new Multiply(new Pow(a, b), new Add(new Multiply(bi, new Ln(a)), new Divide(new Multiply(b, ai), a))),
    (a, b) => ch(a, 0) ? Const.zero : ch(b, 0) ? Const.one : ch(b, 1) ? a : b instanceof Negate ?
        new Divide(Const.one, new Pow(a, b.args[0]).simplify()) : a instanceof Pow ? new Pow(a.args[0],
            new Multiply(a.args[1], b)) : new Pow(a, b))
const Log = make_op('log', (a, b) => Math.log(Math.abs(b)) / Math.log(Math.abs(a)),
    (a, b) => (ai, bi) => ch(a, Math.E) ? new Divide(bi, b) : new Divide(new Subtract(new Divide(new Multiply(
        new Ln(a), bi), b), new Divide(new Multiply(ai, new Ln(b)), a)), new Pow(new Ln(a), Const.two)),
    (a, b) => ch(b, 1) ? Const.zero : eq(a, b) ? Const.one : ch(a, Math.E) ? new Ln(b) : new Log(a, b))
const Mean = make_op('mean', (...w) => w.length === 0 ? 0 : [...w].reduce((wa, wb) => wa + wb, 0) / w.length,
    (...w) => (...wi) => w.length === 0 ? Const.zero : new Divide([...wi].reduce((wai, wbi) => new Add(wai, wbi),
        Const.zero), new Const(w.length)), (...w) => new Mean(...w))
const Var = make_op('var', (...w) => w.length === 0 ? 0 : [...w].reduce((wa, wb) => wa + (wb ** 2), 0)
        / w.length - ([...w].reduce((wa, wb) => wa + wb, 0) / w.length) ** 2,
    (...w) => (...wi) => w.length === 0 ? Const.zero : new Multiply(new Subtract([...w.map((x, i) =>
        [x, wi[i]])].reduce((wa, wb) => new Add(wa, new Multiply(...wb)), Const.zero), new Multiply(
        new Mean(...w), [...wi].reduce((wai, wbi) => new Add(wai, wbi), Const.zero))), new Const(2 / w.length)),
    (...w) => new Var(...w))

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
        this.message = message(...args); }
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