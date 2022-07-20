#include "complex.h"

#include <cmath>

Complex::Complex(const double real, const double imag)
    : m_real(real)
    , m_imag(imag)
{
}

double Complex::real() const
{
    return m_real;
}
double Complex::imag() const
{
    return m_imag;
}
double Complex::abs() const
{
    return std::hypot(m_real, m_imag);
}
std::string Complex::str() const
{
    std::ostringstream ss;
    ss << *this;
    return ss.str();
}

Complex operator-(const Complex & a)
{
    return Complex(-a.real(), -a.imag());
}
Complex operator~(const Complex & a)
{
    return Complex(a.real(), -a.imag());
}

Complex operator+(const Complex & a, const Complex & b)
{
    return Complex(a.real() + b.real(), a.imag() + b.imag());
}
Complex operator-(const Complex & a, const Complex & b)
{
    return Complex(a.real() - b.real(), a.imag() - b.imag());
}
Complex operator*(const Complex & a, const Complex & b)
{
    return Complex(a.real() * b.real() - a.imag() * b.imag(), a.real() * b.imag() + a.imag() * b.real());
}
Complex operator/(const Complex & a, const Complex & b)
{
    const double denominator = b.real() * b.real() + b.imag() * b.imag();
    if (denominator == 0) {
        return Complex(a.real() / 0, a.imag() / 0);
    }
    return Complex((a.real() * b.real() + a.imag() * b.imag()) / denominator, (a.imag() * b.real() - a.real() * b.imag()) / denominator);
}

Complex operator+=(Complex & a, const Complex & b)
{
    return a = a + b;
}
Complex operator-=(Complex & a, const Complex & b)
{
    return a = a - b;
}
Complex operator*=(Complex & a, const Complex & b)
{
    return a = a * b;
}
Complex operator/=(Complex & a, const Complex & b)
{
    return a = a / b;
}

bool operator==(const Complex & a, const Complex & b)
{
    return (a.real() == b.real()) && (a.imag() == b.imag());
}
bool operator!=(const Complex & a, const Complex & b)
{
    return !(a == b);
}

std::ostream & operator<<(std::ostream & out, const Complex & a)
{
    return out << '(' << a.real() << ',' << a.imag() << ')';
}