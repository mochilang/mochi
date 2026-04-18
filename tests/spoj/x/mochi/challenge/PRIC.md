SPOJ PRIC - Prime Sequence (rank 919)

No input. Generates the sequence a[0]=1, a[i]=(a[i-1]+1234567890) mod 2^31 and outputs '1' if each term is prime, '0' otherwise. Builds a sieve of Eratosthenes up to 50,000 for small values, then uses trial division by sieve primes for larger values. Output is batched in chunks of 2000 digits per line for efficiency.
