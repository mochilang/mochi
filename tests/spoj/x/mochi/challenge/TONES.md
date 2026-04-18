# SPOJ TONES - Tone Transposition

https://www.spoj.com/problems/TONES/

Transpose musical chords by t semitones. The 12-note chromatic scale is C C# D D# E F F# G G# A A# B (indices 0–11). Each chord's root is parsed (letter + optional '#'), shifted by t mod 12, and the suffix reattached.

## Approach

Parse each chord: extract root (1 or 2 chars), look up its index in the note array, add t (normalized to 0–11), and output the new root concatenated with the original suffix. Loop until n=0.
