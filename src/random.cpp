#include "random.h"

// pseudo random number state
uint64_t random_state = 6379633040001738036;

uint64_t GetRandomU64Number() {
    // get current state
    uint64_t number = random_state;

    // XOR shift algorithm
    number ^= number << 13;
    number ^= number >> 7;
    number ^= number << 17;

    // update random number state
    random_state = number;

    // return random number
    return number;
}
