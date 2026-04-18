# SPOJ CARL - Carl's Expressions

https://www.spoj.com/problems/CARL/

Evaluate Carl expressions. An expression is either an integer or `(p e1 e2)` where p is a probability. The expected value is `E(e1) + (2p - 1) * E(e2)`. Read until `()` terminates input.

## Approach

Tokenize the input line into `(`, `)`, and word tokens. Evaluate with an explicit stack of frames. Each frame tracks state (waiting for p / e1 / e2), the probability p, and e1. A float value stack holds ready values. At `)`, pop the frame and compute the result, then deliver it into the enclosing frame or the final value stack. Output with 2 decimal places using the standard fmtDecimal helper.
