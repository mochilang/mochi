# SPOJ MWPZ034 - Bucket Tower

https://www.spoj.com/problems/MWPZ034/

Buckets dropped concentrically: a bucket with smaller radius slides inside a larger one and
rests on the highest surface it can land on. Find the total height of the resulting structure.

## Approach

Maintain top[r] = the highest top surface among all buckets dropped with radius exactly r.
When dropping bucket (r, h): scan top[1..r] to find T = max existing height that blocks this
bucket, then set new_top = T + h and update top[r] = max(top[r], new_top). The answer is
the maximum over all top[r].
