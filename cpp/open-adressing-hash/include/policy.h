#pragma once

#include <cstdlib>

struct LinearProbing
{
private:
    mutable std::size_t index;

public:
    LinearProbing()
        : index(0)
    {
    }

    void init() const
    {
        index = 0;
    }

    std::size_t next() const
    {
        return ++index;
    }
};

struct QuadraticProbing
{
private:
    mutable std::size_t index;

public:
    QuadraticProbing()
        : index(0)
    {
    }

    void init() const
    {
        index = 0;
    }

    std::size_t next() const
    {
        ++index;
        return index * index;
    }
};
