# SPOJ INTER - Interbits

https://www.spoj.com/problems/INTER/

Decode a binary transmission where all bits are flipped (0↔1). Each line encodes: 32 bits → n1, 16 bits → n2, n2 bits → n3. Compute r = n3 mod n1. Output r as 32 flipped bits (MSB first).

## Approach

Since n3 can be astronomically large (up to 2^65535 bits), compute n3 mod n1 incrementally: process each bit of n3 left to right, maintaining `r = (r * 2 + bit) % n1`. This keeps r bounded by n1 ≤ 2^32. After decoding (flipping each bit), convert n1 and n2 to integers directly, then stream the n3 bits through the modular accumulator. Finally encode the result as 32 flipped bits.
