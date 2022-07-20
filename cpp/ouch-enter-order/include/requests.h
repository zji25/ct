#pragma once

#include "fields.h"

#include <array>
#include <cstddef>
#include <vector>

enum class RequestType
{
    EnterOrder,
    ReplaceOrder
};

enum class Side
{
    Buy,
    Sell
};

enum class OrdType
{
    Market,
    Limit
};

enum class TimeInForce
{
    Day,
    IOC
};

enum class Capacity
{
    Agency,
    Principal,
    RisklessPrincipal
};

inline unsigned char convert_request_type(const RequestType request_type)
{
    switch (request_type) {
    case RequestType::EnterOrder: return 'O';
    case RequestType::ReplaceOrder: return 'U';
    }
}

inline unsigned char convert_side(const Side side)
{
    switch (side) {
    case Side::Buy: return 'B';
    case Side::Sell: return 'S';
    }
}

inline unsigned char convert_time_in_force(const TimeInForce time_in_force)
{
    switch (time_in_force) {
    case TimeInForce::Day: return '0';
    case TimeInForce::IOC: return '3';
    }
    return 0;
}

inline unsigned char convert_capacity(const Capacity capacity)
{
    switch (capacity) {
    case Capacity::Agency: return '1';
    case Capacity::Principal: return '2';
    case Capacity::RisklessPrincipal: return '7';
    }
}

std::vector<unsigned char> create_enter_order_request(
        const std::string & cl_ord_id,
        Side side,
        double volume,
        double price,
        const std::string & symbol,
        OrdType ord_type,
        TimeInForce time_in_force,
        Capacity capacity,
        const std::string & firm,
        const std::string & user);

std::vector<unsigned char> create_replace_order_request(
        const std::string & old_cl_ord_id,
        const std::string & new_cl_ord_id,
        double total_volume,
        double price,
        TimeInForce time_in_force,
        const std::string & user);
