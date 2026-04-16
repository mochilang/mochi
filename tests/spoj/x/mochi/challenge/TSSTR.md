# SPOJ TSSTR - Shortest Superstring

https://www.spoj.com/problems/TSSTR/

Given n words, find the shortest superstring (string containing all words as substrings). Output "case X Y", the superstring, then for each word its 1-indexed start position in the superstring.

## Approach

1. Remove words that are strict substrings of other (longer) words — they will be covered automatically.
2. Greedy merge: repeatedly find the pair (A, B) with maximum overlap (longest suffix of A = prefix of B), and merge them into A + B[overlap:]. Continue until one string remains.
3. Find each original word's first occurrence in the resulting superstring via linear scan.

Equal-length identical words are not eliminated from the active set during substring removal (since neither strictly dominates the other). Their overlaps cause them to be merged with overlap = full length, producing no extra characters.
