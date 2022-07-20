#pragma once

#include <cstdint>
#include <string>

inline unsigned char * encode(unsigned char * start, const unsigned char c)
{
    *start = c;
    return start + 1;
}

inline unsigned char * encode(unsigned char * start, const uint16_t value)
{
    *start++ = static_cast<unsigned char>(value & 0xFF);
    *start++ = static_cast<unsigned char>((value >> 8) & 0xFF);
    return start;
}

inline unsigned char * encode(unsigned char * start, int64_t value)
{
    for (int i = 0; i < 8; ++i) {
        *start++ = static_cast<unsigned char>(value & 0xFF);
        value >>= 8;
    }
    return start;
}

inline unsigned char * encode(unsigned char * start, const std::string & str, const size_t field_size)
{
    size_t i = 0;
    while (i < str.size() && i < field_size) {
        *start++ = str[i];
        i++;
    }
    while (i < field_size) {
        *start++ = 0x20;
        i++;
    }
    return start;
}

inline unsigned char * encode(unsigned char * start, const uint32_t value, const size_t field_size)
{
    for (int i = field_size - 1; i >= 0; i--) {
        *start++ = (value >> (i * 8) & 0xff);
    }
    return start;
}