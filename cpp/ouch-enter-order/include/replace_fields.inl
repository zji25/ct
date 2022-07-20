#if !defined(FIELD)
#error You need to define FIELD macro
#else
FIELD(time_in_force, alphanumeric, unsigned char, 1, 1)

#undef FIELD

#endif