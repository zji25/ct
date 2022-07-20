#pragma once

#include "complex.h"

#include <functional>
#include <iosfwd>
#include <map>
#include <memory>

class Expression
{
public:
    virtual Complex eval(std::map<std::string, Complex> const & values = {}) const = 0;
    virtual Expression * clone() const = 0;
    friend std::ostream & operator<<(std::ostream & out, const Expression & expression);
    virtual ~Expression() = default;

private:
    virtual std::ostream & to_out(std::ostream & out) const = 0;
};

class Const : public Expression
{
public:
    Const(const Complex & value);

    Complex eval([[maybe_unused]] std::map<std::string, Complex> const & values = {}) const override;
    Const * clone() const override;

private:
    Complex value;
    std::ostream & to_out(std::ostream & out) const override;
};

class Variable : public Expression
{
public:
    Variable(std::string name);

    Complex eval(std::map<std::string, Complex> const & values = {}) const override;
    Variable * clone() const override;

private:
    std::string name;
    std::ostream & to_out(std::ostream & out) const override;
};

template <char C, class F>
class Unary : public Expression
{
public:
    [[maybe_unused]] Unary(Expression const & first)
        : first(first.clone()){};
    [[maybe_unused]] Unary(std::shared_ptr<Expression> const & first)
        : first(first){};

    Complex eval(std::map<std::string, Complex> const & values = {}) const override
    {
        return function(first->eval(values));
    }
    Unary * clone() const override
    {
        return new Unary(*this);
    }

private:
    const std::shared_ptr<Expression> first;
    
    [[no_unique_address]] F function{};
    std::ostream & to_out(std::ostream & out) const override
    {
        return out << '(' << C << *first << ')';
    }
};

template <char C, class F>
class Binary : public Expression
{
public:
    [[maybe_unused]] Binary(Expression const & first,
                            Expression const & second)
        : first(first.clone())
        , second(second.clone()){};
    [[maybe_unused]] Binary(std::shared_ptr<Expression> const & first,
                            std::shared_ptr<Expression> const & second)
        : first(first)
        , second(second){};

    Complex eval(std::map<std::string, Complex> const & values = {}) const override
    {
        return function(first->eval(values), second->eval(values));
    }
    Binary * clone() const override
    {
        return new Binary(*this);
    }

private:
    const std::shared_ptr<Expression> first;
    const std::shared_ptr<Expression> second;

    [[no_unique_address]] F function{};
    std::ostream & to_out(std::ostream & out) const override
    {
        return out << '(' << *first << ' ' << C << ' ' << *second << ')';
    }
};

using Negate = Unary<'-', std::negate<>>;
using Conjugate = Unary<'~', std::bit_not<>>;

using Add = Binary<'+', std::plus<>>;
using Subtract = Binary<'-', std::minus<>>;
using Multiply = Binary<'*', std::multiplies<>>;
using Divide = Binary<'/', std::divides<>>;

Negate operator-(const Expression & first);
Conjugate operator~(const Expression & first);

Add operator+(const Expression & first, const Expression & second);
Subtract operator-(const Expression & first, const Expression & second);
Multiply operator*(const Expression & first, const Expression & second);
Divide operator/(const Expression & first, const Expression & second);