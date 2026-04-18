# SPOJ MPPZF - Piggy Bank

https://www.spoj.com/problems/MPPZF/

Simulate Jaś's piggy bank. Coins have denomination ≤ 10000; banknotes > 10000.
On 'w': find the minimum-denomination coin group. If count ≥ 2, exchange all for count×denom (coin or banknote). If count = 1, take that coin plus the next-minimum coin and exchange their sum.
On 'k': print the maximum banknote value seen, or "bb" if none. Multiple 'k' answers per dataset go on one space-separated line.

## Approach

Maintain a count array `coins[1..10000]`. For 'w', scan from 1 upward to find the minimum occupied denomination. Banknotes are tracked only by their running maximum. On 'k', emit the max banknote or "bb". Collect all 'k' answers per dataset and print them space-separated on one line.
