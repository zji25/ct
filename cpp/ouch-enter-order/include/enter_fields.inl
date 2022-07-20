#if !defined(FIELD)
#error You need to define FIELD macro
#else
FIELD(time_in_force, alphanumeric, unsigned char, 1, 1)
FIELD(capacity, alphanumeric, unsigned char, 1, 8)

#undef FIELD

#endif