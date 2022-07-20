#include "calc.h"

#include <cctype>
#include <cmath>
#include <iostream>

namespace {
const std::size_t max_decimal_digits = 10;
enum class Op
{
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT,
    SIN,
    COS,
    TAN,
    CTN,
    ASIN,
    ACOS,
    ATAN,
    ACTN,
    RAD,
    DEG
};

std::size_t arity(const Op op)
{
    switch (op) {
    case Op::ERR:
    case Op::RAD:
    case Op::DEG:
        return 0;
    case Op::NEG:
    case Op::SQRT:
    case Op::SIN:
    case Op::COS:
    case Op::TAN:
    case Op::CTN:
    case Op::ASIN:
    case Op::ACOS:
    case Op::ATAN:
    case Op::ACTN:
        return 1;
    case Op::SET:
    case Op::ADD:
    case Op::SUB:
    case Op::MUL:
    case Op::DIV:
    case Op::REM:
    case Op::POW:
        return 2;
    }
}

Op parse_op(const std::string & line, std::size_t & i)
{
    const auto rollback = [&i, &line](const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        --i;
        return Op::SET;
    case '+': return Op::ADD;
    case '-': return Op::SUB;
    case '*': return Op::MUL;
    case '/': return Op::DIV;
    case '%': return Op::REM;
    case '_': return Op::NEG;
    case '^': return Op::POW;
    case 'S':
        switch (line[i++]) {
        case 'Q':
            switch (line[i++]) {
            case 'R':
                switch (line[i++]) {
                case 'T': return Op::SQRT;
                default: return rollback(4);
                }
            default: return rollback(3);
            }
        case 'I':
            switch (line[i++]) {
            case 'N': return Op::SIN;
            default: return rollback(3);
            }
        default: return rollback(2);
        }
    case 'C':
        switch (line[i++]) {
        case 'O':
            switch (line[i++]) {
            case 'S': return Op::COS;
            default: rollback(3);
            }
        case 'T':
            switch (line[i++]) {
            case 'N': return Op::CTN;
            default: rollback(3);
            }
        default: rollback(2);
        }
    case 'T':
        switch (line[i++]) {
        case 'A':
            switch (line[i++]) {
            case 'N': return Op::TAN;
            default: rollback(3);
            }
        default: rollback(2);
        }
    case 'A':
        switch (line[i++]) {
        case 'S':
            switch (line[i++]) {
            case 'I':
                switch (line[i++]) {
                case 'N': return Op::ASIN;
                default: rollback(4);
                }
            default: rollback(3);
            }
        case 'C':
            switch (line[i++]) {
            case 'O':
                switch (line[i++]) {
                case 'S': return Op::ACOS;
                default: rollback(4);
                }
            case 'T':
                switch (line[i++]) {
                case 'N': return Op::ACTN;
                default: rollback(4);
                }
            default: rollback(3);
            }
        case 'T':
            switch (line[i++]) {
            case 'A':
                switch (line[i++]) {
                case 'N': return Op::ATAN;
                default: rollback(4);
                }
            default: rollback(3);
            }
        default: rollback(2);
        }
    case 'R':
        switch (line[i++]) {
        case 'A':
            switch (line[i++]) {
            case 'D': return Op::RAD;
            default: rollback(3);
            }
        default: rollback(2);
        }
    case 'D':
        switch (line[i++]) {
        case 'E':
            switch (line[i++]) {
            case 'G': return Op::DEG;
            default: rollback(3);
            }
        default: rollback(2);
        }
    default: return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string & line, std::size_t & i)
{
    double res = 0;
    std::size_t count = 0;
    bool good = true;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (integer) {
                res *= 10;
                res += line[i] - '0';
            }
            else {
                fraction /= 10;
                res += (line[i] - '0') * fraction;
            }
            ++i;
            ++count;
            break;
        case '.':
            integer = false;
            ++i;
            break;
        default:
            good = false;
            break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << '\'' << std::endl;
    }
    else if (i < line.size()) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << '\'' << std::endl;
    }
    return res;
}

double to_deg(const double value, const bool rad_on)
{
    return rad_on ? value : value / M_PI * 180;
}

double to_rad(const double value, const bool rad_on)
{
    return rad_on ? value : value * M_PI / 180;
}

void bad_arg_for_unary(const std::string & name, const double current)
{
    std::cerr << "Bad argument for " << name << ": " << current << std::endl;
}

bool in_interval(const double a, const double b, const double current)
{
    return a <= current && current <= b;
}

double unary(const double current, const Op op, const bool rad_on)
{
    switch (op) {
    case Op::NEG: return -current;
    case Op::SQRT:
        if (current > 0) {
            return std::sqrt(current);
        }
        else {
            bad_arg_for_unary("SQRT", current);
            return current;
        }
    case Op::SIN: return std::sin(to_rad(current, rad_on));
    case Op::COS: return std::cos(to_rad(current, rad_on));
    case Op::TAN: return std::tan(to_rad(current, rad_on));
    case Op::CTN: {
        const double s = std::sin(to_rad(current, rad_on));
        return s != 0 ? std::cos(to_rad(current, rad_on)) / s : INFINITY;
    }
    case Op::ASIN: {
        if (in_interval(-1, 1, current)) {
            return to_deg(std::asin(current), rad_on);
        }
        bad_arg_for_unary("ASIN", current);
        return current;
    }
    case Op::ACOS:
        if (in_interval(-1, 1, current)) {
            return to_deg(std::acos(current), rad_on);
        }
        bad_arg_for_unary("ACOS", current);
        return current;
    case Op::ATAN: return to_deg(std::atan(current), rad_on);
    case Op::ACTN: return to_deg(M_PI / 2 - std::atan(current), rad_on);
    default: return current;
    }
}

double binary(const Op op, const double left, const double right)
{
    switch (op) {
    case Op::SET: return right;
    case Op::ADD: return left + right;
    case Op::SUB: return left - right;
    case Op::MUL: return left * right;
    case Op::DIV:
        if (right != 0) {
            return left / right;
        }
        else {
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return left;
        }
    case Op::REM:
        if (right != 0) {
            return std::fmod(left, right);
        }
        else {
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return left;
        }
    case Op::POW: return std::pow(left, right);
    default: return left;
    }
}
} // namespace
double process_line(const double current, bool & rad_on, const std::string & line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op)) {
    case 2: {
        i = skip_ws(line, i);
        const auto old_i = i;
        const auto arg = parse_arg(line, i);
        if (i == old_i) {
            std::cerr << "No argument for a binary operation" << std::endl;
            break;
        }
        else if (i < line.size()) {
            break;
        }
        return binary(op, current, arg);
    }
    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << '\'' << std::endl;
            break;
        }
        return unary(current, op, rad_on);
    }
    case 0:
        if (op == Op::RAD) {
            rad_on = true;
        }
        else if (op == Op::DEG) {
            rad_on = false;
        }
        return current;
    default: return current;
    }
    return current;
}
