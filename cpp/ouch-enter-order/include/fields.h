#pragma once

#include "encoders.h"

#include <cmath>

inline unsigned char * encode_text(unsigned char * start, const std::string & str, const size_t field_size)
{
    return encode(start, str, field_size);
}

inline unsigned char * encode_char(unsigned char * start, const char c)
{
    return encode(start, static_cast<uint8_t>(c));
}

inline unsigned char * encode_integer(unsigned char * start, uint32_t digit, const size_t field_size)
{
    return encode(start, digit, field_size);
}

inline constexpr size_t alphanumeric_size = 1;

inline unsigned char * encode_alphanumeric(unsigned char * start, unsigned const char c)
{
    return encode_char(start, c);
}

#define FIELD(name, protocol_type, ctype, _, __)                                         \
    inline unsigned char * encode_field_##name(unsigned char * start, const ctype value) \
    {                                                                                    \
        return encode_##protocol_type(start, value);                                     \
    }
#include "enter_fields.inl"

#define FIELD(name, protocol_type, _, __, ___) inline constexpr size_t name##_field_size = protocol_type##_size;

#include "enter_fields.inl"

inline void set_opt_field_bit(unsigned char * bitfield_start, unsigned bitfield_num, unsigned bit)
{
    *(bitfield_start + bitfield_num - 1) |= bit;
}
