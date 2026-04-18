# SPOJ HWORK - Homework

https://www.spoj.com/problems/HWORK/

Johnny writes one text T, copies it k times, then erases some characters from each copy to
obtain the required words. Every word must appear as a substring of T. Score = sum of word
lengths - len(T). Maximize score by finding the shortest superstring covering all n words.

## Approach

Greedy shortest superstring:
1. Remove words that are substrings of another word (they appear in T for free).
2. Among the remaining words, greedily merge the pair with the greatest suffix-prefix
   overlap, replacing the pair with the merged string.
3. Repeat until one string remains — that is T.
4. For each original word, output its 1-based start position in T.

Output per test case: "case C Y", then T, then one position per word.
