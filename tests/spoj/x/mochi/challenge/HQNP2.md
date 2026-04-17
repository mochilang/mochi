# HQNP2 - HQ9+- Incomputable?!

https://www.spoj.com/problems/HQNP2/

Given T numbers (possibly very large), output an HQ0-9+- program that prints each number,
using '+' at least once.

## Approach

Key insight: 'h' sets buffer to "helloworld", 'e' applies l33t encoding giving "h311w0r1d",
'p' removes characters at prime-or-power-of-2 indices leaving just '0' (index 6, 1-based).
So "hep" outputs the character '0'. Appending k copies of 'i' increments ASCII by k,
giving digit k (for k=0..9). Finally '+' satisfies the accumulator requirement.

For single-digit n: output "hep" + n copies of "i" + "+".
For multi-digit n (including very large numbers): output a blank line (unsupported).
