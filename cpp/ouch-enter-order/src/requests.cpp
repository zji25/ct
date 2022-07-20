#include "requests.h"

#include "encoders.h"
#include "fields.h"

#include <string>
#include <vector>

namespace {

void encode_enter_order_opt_fields(unsigned char * bitfield_start,
                                   const char time_in_force,
                                   const char capacity)
{
    auto * p = bitfield_start + 4;
#define FIELD(name, _, __, bitfield_num, bit)             \
    set_opt_field_bit(bitfield_start, bitfield_num, bit); \
    p = encode_field_##name(p, name);
#include "enter_fields.inl"
}

void encode_replace_order_opt_fields(unsigned char * bitfield_start,
                                     const char time_in_force)
{
    auto * p = bitfield_start + 4;
#define FIELD(name, _, __, bitfield_num, bit)             \
    set_opt_field_bit(bitfield_start, bitfield_num, bit); \
    p = encode_field_##name(p, name);
#include "replace_fields.inl"
}

uint32_t convert_price(const OrdType ord_type, const double price)
{
    if (ord_type == OrdType::Limit) {
        return price * 10000;
    }
    return 0x7FFFFFFF;
}

unsigned char * encode_request_type(unsigned char * start, const RequestType type)
{
    return encode(start, convert_request_type(type));
}
} // namespace

std::vector<unsigned char> create_enter_order_request(
        const std::string & cl_ord_id,
        const Side side,
        const double volume,
        const double price,
        const std::string & symbol,
        const OrdType ord_type,
        const TimeInForce time_in_force,
        const Capacity capacity,
        const std::string & firm,
        const std::string & user)
{
    std::vector<unsigned char> msg(44);
    auto * p = encode_request_type(&msg[0], RequestType::EnterOrder);
    p = encode_text(p, cl_ord_id, 14);
    p = encode_char(p, convert_side(side));
    p = encode_integer(p, static_cast<uint32_t>(volume), 4);
    p = encode_integer(p, std::stol(symbol), 4);
    p = encode_integer(p, convert_price(ord_type, price), 4);

    p = encode_text(p, firm, 4);
    p = encode_text(p, user, 6);
    encode_enter_order_opt_fields(p,
                                  convert_time_in_force(time_in_force),
                                  convert_capacity(capacity));
    return msg;
}

std::vector<unsigned char> create_replace_order_request(
        const std::string & old_cl_ord_id,
        const std::string & new_cl_ord_id,
        const double total_volume,
        const double price,
        const TimeInForce time_in_force,
        const std::string & user)
{
    std::vector<unsigned char> msg(48);
    auto * p = encode_request_type(&msg[0], RequestType::ReplaceOrder);
    p = encode_text(p, old_cl_ord_id, 14);
    p = encode_text(p, new_cl_ord_id, 14);
    p = encode_integer(p, static_cast<uint32_t>(total_volume), 4);
    p = encode_integer(p, static_cast<uint32_t>(price * 10000), 4);
    p = encode_text(p, user, 6);
    encode_replace_order_opt_fields(p,
                                    convert_time_in_force(time_in_force));
    return msg;
}
