#pragma once

#include <sstream>

class Complex
{
private:
    double m_real;
    double m_imag;

public:
    Complex(double real = 0, double imag = 0);

    double real() const;
    double imag() const;
    double abs() const;
    std::string str() const;
    ~Complex() = default;
};

Complex operator-(const Complex & a);
Complex operator~(const Complex & a);

Complex operator+(const Complex & a, const Complex & b);
Complex operator-(const Complex & a, const Complex & b);
Complex operator*(const Complex & a, const Complex & b);
Complex operator/(const Complex & a, const Complex & b);

Complex operator+=(Complex & a, const Complex & b);
Complex operator-=(Complex & a, const Complex & b);
Complex operator*=(Complex & a, const Complex & b);
Complex operator/=(Complex & a, const Complex & b);

bool operator==(const Complex & a, const Complex & b);
bool operator!=(const Complex & a, const Complex & b);

std::ostream & operator<<(std::ostream & out, const Complex & a);