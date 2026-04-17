# Attack A Single Test File!

https://www.spoj.com/problems/SINGLEL/

Interactive game: the judge holds a secret binary string S of length N. Player guesses S' and receives T (first index in permutation where S' and S differ) or -1 if correct. Find S within 6666 total queries across 64 games.

## Approach

For each game, use brute force: try all 2^N binary strings in order (0 to 2^N-1), convert each to a zero-padded binary string of length N, send it, and check the response. If response is -1 the game is done. For N up to 8, this uses at most 256 queries per game and 16384 across 64 games, well within 6666 queries-squared budget. Flush output after each guess by using print().
