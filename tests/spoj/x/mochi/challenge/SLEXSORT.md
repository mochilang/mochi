# SPOJ SLEXSORT - Sorting by a Custom Alphabet

https://www.spoj.com/problems/SLEXSORT/

Sort words using a custom alphabet ordering. Score = source code length (shorter = better rank).

## Approach

For each test: build a rank map from the custom alphabet string (rank+1 stored so 0 = absent). Compare words character by character using ranks; shorter prefix-match word is smaller. Insertion sort the word list. Print sorted words followed by a blank line.
