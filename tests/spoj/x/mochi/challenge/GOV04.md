# GOV04

https://www.spoj.com/problems/GOV04/

Given R, find a,b with |a|<=R, |b|<=R maximizing consecutive primes in n^2+an+b for n=0,1,... Output "count @ |a*b|". Ties: smallest a, then smallest b.

Uses a sieve of Eratosthenes up to 2M for fast primality checks, then brute force over all (a, prime b) pairs.
