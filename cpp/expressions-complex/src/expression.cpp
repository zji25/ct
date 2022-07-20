#include "expression.h"

#include <ostream>
#include <string>

std::ostream & operator<<(std::ostream & out, const Expression & expression)
{
    return expression.to_out(out);
}

Const::Const(const Complex & value)
    : value(value)
{
}
Complex Const::eval([[maybe_unused]] const std::map<std::string, Complex> & values) const
{
    return value;
}
Const * Const::clone() const
{
    return new Const(value);
}
std::ostream & Const::to_out(std::ostream & out) const
{
    return out << value;
}

Variable::Variable(std::string name)
    : name(std::move(name))
{
}
Complex Variable::eval(const std::map<std::string, Complex> & values) const
{
    return values.at(name);
}
Variable * Variable::clone() const
{
    return new Variable(name);
}
std::ostream & Variable::to_out(std::ostream & out) const
{
    return out << name;
}

Negate operator-(const Expression & first)
{
    return Negate(first);
}
Conjugate operator~(const Expression & first)
{
    return Conjugate(first);
}

Add operator+(const Expression & first, const Expression & second)
{
    return Add(first, second);
}
Subtract operator-(const Expression & first, const Expression & second)
{
    return Subtract(first, second);
}
Multiply operator*(const Expression & first, const Expression & second)
{
    return Multiply(first, second);
}
Divide operator/(const Expression & first, const Expression & second)
{
    return Divide(first, second);
}
