# Scala Transpiler Rosetta Output

Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.

## Golden Test Checklist (229/493)
_Last updated: 2025-07-28 10:36 +0700_

| Index | Name | Status | Duration | Memory |
|------:|------|:-----:|---------:|-------:|
| 1 | 100-doors-2 | ✓ | 6µs | 8.7 MB |
| 2 | 100-doors-3 | ✓ | 5µs | 8.7 MB |
| 3 | 100-doors | ✓ | 8µs | 8.7 MB |
| 4 | 100-prisoners | ✓ | 4.792ms | 8.7 MB |
| 5 | 15-puzzle-game | ✓ | 21µs | 8.7 MB |
| 6 | 15-puzzle-solver | ✓ | 3µs | 8.7 MB |
| 7 | 2048 | ✓ | 19µs | 8.7 MB |
| 8 | 21-game | ✓ | 7µs | 8.7 MB |
| 9 | 24-game-solve | ✓ | 39µs | 8.8 MB |
| 10 | 24-game | ✓ | 13µs | 8.8 MB |
| 11 | 4-rings-or-4-squares-puzzle | ✓ | 62µs | 85.8 KB |
| 12 | 9-billion-names-of-god-the-integer | ✓ | 2.573ms | 8.7 MB |
| 13 | 99-bottles-of-beer-2 | ✓ | 54µs | 8.7 MB |
| 14 | 99-bottles-of-beer | ✓ | 14µs | 8.7 MB |
| 15 | DNS-query | ✓ | 369µs | 8.7 MB |
| 16 | Duffinian-numbers |   |  |  |
| 17 | Find-if-a-point-is-within-a-triangle |   |  |  |
| 18 | a+b | ✓ | 6µs | 8.7 MB |
| 19 | abbreviations-automatic | ✓ | 34µs | 8.7 MB |
| 20 | abbreviations-easy | ✓ | 14µs | 8.7 MB |
| 21 | abbreviations-simple | ✓ | 18µs | 8.7 MB |
| 22 | abc-problem | ✓ | 13µs | 8.7 MB |
| 23 | abelian-sandpile-model-identity | ✓ | 10µs | 8.7 MB |
| 24 | abelian-sandpile-model | ✓ | 7µs | 8.7 MB |
| 25 | abstract-type | ✓ | 3µs | 8.7 MB |
| 26 | abundant-deficient-and-perfect-number-classifications | ✓ | 255µs | 8.7 MB |
| 27 | abundant-odd-numbers | ✓ | 654µs | 8.7 MB |
| 28 | accumulator-factory | ✓ | 4µs | 8.7 MB |
| 29 | achilles-numbers | ✓ | 65µs | 8.7 MB |
| 30 | ackermann-function-2 | ✓ | 3µs | 8.7 MB |
| 31 | ackermann-function-3 | ✓ |  |  |
| 32 | ackermann-function | ✓ | 4µs | 8.7 MB |
| 33 | active-directory-connect | ✓ | 4µs | 8.7 MB |
| 34 | active-directory-search-for-a-user | ✓ | 4µs | 8.7 MB |
| 35 | active-object | ✓ | 39µs | 8.7 MB |
| 36 | add-a-variable-to-a-class-instance-at-runtime | ✓ | 5µs | 8.7 MB |
| 37 | additive-primes | ✓ | 6µs | 8.7 MB |
| 38 | address-of-a-variable | ✓ | 8µs | 8.7 MB |
| 39 | adfgvx-cipher | ✓ | 15µs | 8.7 MB |
| 40 | aks-test-for-primes | ✓ | 5µs | 8.7 MB |
| 41 | algebraic-data-types | ✓ | 7µs | 8.7 MB |
| 42 | align-columns | ✓ | 9µs | 8.7 MB |
| 43 | aliquot-sequence-classifications | ✓ | 20µs | 8.7 MB |
| 44 | almkvist-giullera-formula-for-pi | ✓ | 252µs | 8.7 MB |
| 45 | almost-prime | ✓ | 4µs | 8.7 MB |
| 46 | amb | ✓ | 3µs | 8.7 MB |
| 47 | amicable-pairs | ✓ | 388µs | 8.7 MB |
| 48 | anagrams-deranged-anagrams | ✓ | 7µs | 8.7 MB |
| 49 | anagrams | ✓ | 10µs | 8.7 MB |
| 50 | angle-difference-between-two-bearings-1 | ✓ | 5µs | 8.7 MB |
| 51 | angle-difference-between-two-bearings-2 | ✓ | 5µs | 8.7 MB |
| 52 | angles-geometric-normalization-and-conversion | ✓ | 9µs | 8.8 MB |
| 53 | animate-a-pendulum | ✓ | 17µs | 8.7 MB |
| 54 | animation | ✓ | 8µs | 8.7 MB |
| 55 | anonymous-recursion-1 | ✓ | 3µs | 8.7 MB |
| 56 | anonymous-recursion-2 | ✓ | 3µs | 8.7 MB |
| 57 | anonymous-recursion | ✓ | 2µs | 8.7 MB |
| 58 | anti-primes | ✓ | 31µs | 8.7 MB |
| 59 | append-a-record-to-the-end-of-a-text-file | ✓ | 2µs | 8.7 MB |
| 60 | apply-a-callback-to-an-array-1 | ✓ | 4µs | 8.7 MB |
| 61 | apply-a-callback-to-an-array-2 | ✓ | 4µs | 8.7 MB |
| 62 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 8µs | 8.7 MB |
| 63 | approximate-equality | ✓ | 5µs | 8.7 MB |
| 64 | arbitrary-precision-integers-included- | ✓ | 237µs | 105.4 KB |
| 65 | archimedean-spiral | ✓ | 261µs | 8.7 MB |
| 66 | arena-storage-pool | ✓ | 7µs | 8.7 MB |
| 67 | arithmetic-complex | ✓ | 5µs | 8.7 MB |
| 68 | arithmetic-derivative | ✓ | 14µs | 8.7 MB |
| 69 | arithmetic-evaluation | ✓ | 4µs | 8.7 MB |
| 70 | arithmetic-geometric-mean-calculate-pi | ✓ | 4µs | 8.7 MB |
| 71 | arithmetic-geometric-mean | ✓ | 3µs | 8.7 MB |
| 72 | arithmetic-integer-1 | ✓ | 2µs | 8.7 MB |
| 73 | arithmetic-integer-2 | ✓ | 2µs | 8.7 MB |
| 74 | arithmetic-numbers | ✓ |  |  |
| 75 | arithmetic-rational | ✓ | 6µs | 8.7 MB |
| 76 | array-concatenation | ✓ | 4µs | 8.7 MB |
| 77 | array-length | ✓ | 2µs | 8.7 MB |
| 78 | arrays | ✓ | 5µs | 8.7 MB |
| 79 | ascending-primes | ✓ | 7µs | 8.7 MB |
| 80 | ascii-art-diagram-converter | ✓ | 3µs | 8.7 MB |
| 81 | assertions | ✓ | 2µs | 8.7 MB |
| 82 | associative-array-creation | ✓ | 7µs | 8.7 MB |
| 83 | associative-array-iteration | ✓ | 4µs | 3.5 MB |
| 84 | associative-array-merging | ✓ | 5µs | 3.5 MB |
| 85 | atomic-updates | ✓ | 10µs | 3.5 MB |
| 86 | attractive-numbers | ✓ | 1µs | 3.5 MB |
| 87 | average-loop-length | ✓ | 170µs | 3.5 MB |
| 88 | averages-arithmetic-mean | ✓ | 9µs | 3.5 MB |
| 89 | averages-mean-time-of-day | ✓ | 24µs | 3.5 MB |
| 90 | averages-median-1 | ✓ | 4µs | 3.5 MB |
| 91 | averages-median-2 | ✓ | 4µs | 3.5 MB |
| 92 | averages-median-3 | ✓ | 8µs | 3.5 MB |
| 93 | averages-mode | ✓ | 10µs | 3.5 MB |
| 94 | averages-pythagorean-means | ✓ | 10µs | 3.5 MB |
| 95 | averages-root-mean-square | ✓ | 1µs | 3.5 MB |
| 96 | averages-simple-moving-average | ✓ | 6µs | 3.5 MB |
| 97 | avl-tree | ✓ | 10µs | 3.5 MB |
| 98 | b-zier-curves-intersections | ✓ | 56µs | 3.5 MB |
| 99 | babbage-problem | ✓ | 4µs | 3.5 MB |
| 100 | babylonian-spiral | ✓ | 19µs | 3.5 MB |
| 101 | balanced-brackets | ✓ | 8µs | 3.5 MB |
| 102 | balanced-ternary | ✓ | 5µs | 3.5 MB |
| 103 | barnsley-fern | ✓ | 31µs | 3.5 MB |
| 104 | base64-decode-data | ✓ | 9µs | 3.5 MB |
| 105 | bell-numbers | ✓ | 17µs | 3.5 MB |
| 106 | benfords-law | ✓ | 29µs | 3.5 MB |
| 107 | bernoulli-numbers | ✓ | 92µs | 3.5 MB |
| 108 | best-shuffle | ✓ | 6µs | 3.5 MB |
| 109 | bifid-cipher | ✓ | 14µs | 3.5 MB |
| 110 | bin-given-limits | ✓ | 10µs | 3.5 MB |
| 111 | binary-digits | ✓ | 2µs | 3.5 MB |
| 112 | binary-search | ✓ | 5µs | 3.5 MB |
| 113 | binary-strings |   |  |  |
| 114 | bioinformatics-base-count | ✓ | 4µs | 8.7 MB |
| 115 | bioinformatics-global-alignment | ✓ | 199µs | 8.7 MB |
| 116 | bioinformatics-sequence-mutation | ✓ | 21µs | 8.7 MB |
| 117 | biorhythms | ✓ | 11µs | 8.7 MB |
| 118 | bitcoin-address-validation | ✓ | 25µs | 8.9 MB |
| 119 | bitmap-b-zier-curves-cubic | error |  |  |
| 120 | bitmap-b-zier-curves-quadratic | error |  |  |
| 121 | bitmap-bresenhams-line-algorithm | ✓ | 7µs | 8.7 MB |
| 122 | bitmap-flood-fill | ✓ | 4µs | 8.7 MB |
| 123 | bitmap-histogram | ✓ | 5µs | 8.7 MB |
| 124 | bitmap-midpoint-circle-algorithm | ✓ | 28µs | 8.7 MB |
| 125 | bitmap-ppm-conversion-through-a-pipe | ✓ | 81µs | 8.7 MB |
| 126 | bitmap-read-a-ppm-file | error |  |  |
| 127 | bitmap-read-an-image-through-a-pipe | error |  |  |
| 128 | bitmap-write-a-ppm-file | ✓ | 6µs | 8.7 MB |
| 129 | bitmap | error |  |  |
| 130 | bitwise-io-1 | ✓ | 3µs | 8.7 MB |
| 131 | bitwise-io-2 | error |  |  |
| 132 | bitwise-operations | ✓ | 6µs | 8.7 MB |
| 133 | blum-integer | ✓ | 7µs | 8.7 MB |
| 134 | boolean-values | ✓ | 6µs | 8.7 MB |
| 135 | box-the-compass | ✓ | 7µs | 8.7 MB |
| 136 | boyer-moore-string-search | ✓ | 39µs | 8.7 MB |
| 137 | brazilian-numbers | ✓ | 2.52ms | 8.7 MB |
| 138 | break-oo-privacy | ✓ | 3µs | 8.7 MB |
| 139 | brilliant-numbers | error |  |  |
| 140 | brownian-tree | ✓ |  |  |
| 141 | bulls-and-cows-player | ✓ | 39µs | 8.7 MB |
| 142 | bulls-and-cows | error |  |  |
| 143 | burrows-wheeler-transform | ✓ | 25µs | 8.7 MB |
| 144 | caesar-cipher-1 | ✓ | 8µs | 8.7 MB |
| 145 | caesar-cipher-2 | ✓ | 11µs | 8.7 MB |
| 146 | calculating-the-value-of-e | error |  |  |
| 147 | calendar---for-real-programmers-1 | ✓ | 56µs | 8.7 MB |
| 148 | calendar---for-real-programmers-2 | ✓ | 47µs | 8.7 MB |
| 149 | calendar | ✓ | 54µs | 8.7 MB |
| 150 | calkin-wilf-sequence | error |  |  |
| 151 | call-a-foreign-language-function | ✓ | 2µs | 8.7 MB |
| 152 | call-a-function-1 | ✓ |  | 8.7 MB |
| 153 | call-a-function-10 | ✓ | 1µs | 8.7 MB |
| 154 | call-a-function-11 | ✓ | 3µs | 8.7 MB |
| 155 | call-a-function-12 | ✓ | 3µs | 8.7 MB |
| 156 | call-a-function-2 | ✓ | 3µs | 8.7 MB |
| 157 | call-a-function-3 | ✓ | 7µs | 8.7 MB |
| 158 | call-a-function-4 | ✓ | 1µs | 8.7 MB |
| 159 | call-a-function-5 | ✓ | 3µs | 8.7 MB |
| 160 | call-a-function-6 | ✓ | 9µs | 8.7 MB |
| 161 | call-a-function-7 | ✓ |  | 8.7 MB |
| 162 | call-a-function-8 | error |  |  |
| 163 | call-a-function-9 | ✓ | 3µs | 8.7 MB |
| 164 | call-an-object-method-1 | ✓ | 1µs | 8.7 MB |
| 165 | call-an-object-method-2 | ✓ | 2µs | 8.7 MB |
| 166 | call-an-object-method-3 | ✓ | 1µs | 8.7 MB |
| 167 | call-an-object-method | ✓ | 1µs | 8.7 MB |
| 168 | camel-case-and-snake-case | error |  |  |
| 169 | canny-edge-detector | error |  |  |
| 170 | canonicalize-cidr | error |  |  |
| 171 | cantor-set | error |  |  |
| 172 | carmichael-3-strong-pseudoprimes | error |  |  |
| 173 | cartesian-product-of-two-or-more-lists-1 | ✓ | 4µs | 8.7 MB |
| 174 | cartesian-product-of-two-or-more-lists-2 | ✓ | 9µs | 8.7 MB |
| 175 | cartesian-product-of-two-or-more-lists-3 | ✓ | 10µs | 8.7 MB |
| 176 | cartesian-product-of-two-or-more-lists-4 | ✓ | 10µs | 8.7 MB |
| 177 | case-sensitivity-of-identifiers | ✓ | 5µs | 8.7 MB |
| 178 | casting-out-nines | error |  |  |
| 179 | catalan-numbers-1 | ✓ | 21µs | 8.7 MB |
| 180 | catalan-numbers-2 | ✓ | 16µs | 8.7 MB |
| 181 | catalan-numbers-pascals-triangle | error |  |  |
| 182 | catamorphism | ✓ | 3µs | 8.7 MB |
| 183 | catmull-clark-subdivision-surface | error |  |  |
| 184 | chaocipher | ✓ | 4µs | 8.7 MB |
| 185 | chaos-game | error |  |  |
| 186 | character-codes-1 | ✓ | 2µs | 8.7 MB |
| 187 | character-codes-2 | ✓ | 2µs | 8.7 MB |
| 188 | character-codes-3 | ✓ | 2µs | 8.7 MB |
| 189 | character-codes-4 | ✓ | 2µs | 8.7 MB |
| 190 | character-codes-5 | ✓ | 2µs | 8.7 MB |
| 191 | chat-server | ✓ | 3µs | 8.7 MB |
| 192 | check-machin-like-formulas | ✓ | 33µs | 8.7 MB |
| 193 | check-that-file-exists | ✓ | 3µs | 8.7 MB |
| 194 | checkpoint-synchronization-1 | ✓ | 17µs | 8.7 MB |
| 195 | checkpoint-synchronization-2 | ✓ | 16µs | 8.7 MB |
| 196 | checkpoint-synchronization-3 | ✓ | 17µs | 8.7 MB |
| 197 | checkpoint-synchronization-4 | ✓ | 20µs | 8.7 MB |
| 198 | chernicks-carmichael-numbers | ✓ | 2.23ms | 8.7 MB |
| 199 | cheryls-birthday | ✓ | 5µs | 8.7 MB |
| 200 | chinese-remainder-theorem | ✓ | 22µs | 8.7 MB |
| 201 | chinese-zodiac | ✓ | 4µs | 8.7 MB |
| 202 | cholesky-decomposition-1 | error |  |  |
| 203 | cholesky-decomposition | ✓ |  |  |
| 204 | chowla-numbers | ✓ |  |  |
| 205 | church-numerals-1 | ✓ |  |  |
| 206 | church-numerals-2 | error |  |  |
| 207 | circles-of-given-radius-through-two-points | error |  |  |
| 208 | circular-primes | error |  |  |
| 209 | cistercian-numerals | error |  |  |
| 210 | comma-quibbling | ✓ |  |  |
| 211 | compiler-virtual-machine-interpreter | error |  |  |
| 212 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k | error |  |  |
| 213 | compound-data-type | ✓ |  |  |
| 214 | concurrent-computing-1 | error |  |  |
| 215 | concurrent-computing-2 | error |  |  |
| 216 | concurrent-computing-3 | error |  |  |
| 217 | conditional-structures-1 | ✓ |  |  |
| 218 | conditional-structures-10 | ✓ |  |  |
| 219 | conditional-structures-2 | ✓ |  |  |
| 220 | conditional-structures-3 | ✓ |  |  |
| 221 | conditional-structures-4 | ✓ |  |  |
| 222 | conditional-structures-5 | ✓ |  |  |
| 223 | conditional-structures-6 | error |  |  |
| 224 | conditional-structures-7 | ✓ |  | 8.7 MB |
| 225 | conditional-structures-8 | ✓ |  | 8.7 MB |
| 226 | conditional-structures-9 | ✓ |  | 8.7 MB |
| 227 | consecutive-primes-with-ascending-or-descending-differences | error |  |  |
| 228 | constrained-genericity-1 | ✓ |  | 8.7 MB |
| 229 | constrained-genericity-2 | error |  |  |
| 230 | constrained-genericity-3 | ✓ |  | 8.7 MB |
| 231 | constrained-genericity-4 | ✓ | 3µs | 8.7 MB |
| 232 | constrained-random-points-on-a-circle-1 | error |  |  |
| 233 | constrained-random-points-on-a-circle-2 | error |  |  |
| 234 | continued-fraction | ✓ | 10µs | 8.7 MB |
| 235 | convert-decimal-number-to-rational | error |  |  |
| 236 | convert-seconds-to-compound-duration | ✓ | 3µs | 8.7 MB |
| 237 | convex-hull | ✓ | 21µs | 8.7 MB |
| 238 | conways-game-of-life | ✓ | 851µs | 8.7 MB |
| 239 | copy-a-string-1 | ✓ |  | 8.7 MB |
| 240 | copy-a-string-2 | ✓ | 3µs | 8.7 MB |
| 241 | copy-stdin-to-stdout-1 | ✓ | 15µs | 8.7 MB |
| 242 | copy-stdin-to-stdout-2 | ✓ | 3µs | 8.7 MB |
| 243 | count-in-factors | error |  |  |
| 244 | count-in-octal-1 | ✓ | 23µs | 8.8 MB |
| 245 | count-in-octal-2 | ✓ | 245µs | 8.7 MB |
| 246 | count-in-octal-3 | error |  |  |
| 247 | count-in-octal-4 | ✓ | 16µs | 8.8 MB |
| 248 | count-occurrences-of-a-substring | error |  |  |
| 249 | count-the-coins-1 | ✓ | 3µs | 8.7 MB |
| 250 | count-the-coins-2 | ✓ | 33µs | 8.7 MB |
| 251 | cramers-rule | error |  |  |
| 252 | crc-32-1 | error |  |  |
| 253 | crc-32-2 | error |  |  |
| 254 | create-a-file-on-magnetic-tape | ✓ | 2µs | 8.7 MB |
| 255 | create-a-file | ✓ | 2µs | 8.7 MB |
| 256 | create-a-two-dimensional-array-at-runtime-1 | ✓ | 4µs | 8.7 MB |
| 257 | create-an-html-table | ✓ | 16µs | 8.7 MB |
| 258 | create-an-object-at-a-given-address | ✓ | 20µs | 8.7 MB |
| 259 | csv-data-manipulation | ✓ | 11µs | 8.7 MB |
| 260 | csv-to-html-translation-1 | ✓ | 3µs | 8.7 MB |
| 261 | csv-to-html-translation-2 | ✓ | 5µs | 8.7 MB |
| 262 | csv-to-html-translation-3 | ✓ | 2µs | 8.7 MB |
| 263 | csv-to-html-translation-4 | ✓ | 2µs | 8.7 MB |
| 264 | csv-to-html-translation-5 | ✓ | 6µs | 8.7 MB |
| 265 | cuban-primes | ✓ | 13.374ms | 8.7 MB |
| 266 | cullen-and-woodall-numbers | ✓ | 6µs | 8.7 MB |
| 267 | cumulative-standard-deviation | ✓ | 6µs | 8.7 MB |
| 268 | currency | ✓ | 7µs | 8.7 MB |
| 269 | currying | ✓ | 6µs | 8.7 MB |
| 270 | curzon-numbers | ✓ | 414µs | 8.7 MB |
| 271 | cusip | error |  |  |
| 272 | cyclops-numbers |   |  |  |
| 273 | damm-algorithm | ✓ | 8µs | 8.7 MB |
| 274 | date-format |   |  |  |
| 275 | date-manipulation |   |  |  |
| 276 | day-of-the-week |   |  |  |
| 277 | de-bruijn-sequences |   |  |  |
| 278 | deal-cards-for-freecell |   |  |  |
| 279 | death-star |   |  |  |
| 280 | deceptive-numbers |   |  |  |
| 281 | deconvolution-1d-2 |   |  |  |
| 282 | deconvolution-1d-3 |   |  |  |
| 283 | deconvolution-1d |   |  |  |
| 284 | deepcopy-1 |   |  |  |
| 285 | define-a-primitive-data-type |   |  |  |
| 286 | delegates |   |  |  |
| 287 | demings-funnel |   |  |  |
| 288 | department-numbers |   |  |  |
| 289 | descending-primes |   |  |  |
| 290 | detect-division-by-zero |   |  |  |
| 291 | determine-if-a-string-has-all-the-same-characters |   |  |  |
| 292 | determine-if-a-string-has-all-unique-characters |   |  |  |
| 293 | determine-if-a-string-is-collapsible |   |  |  |
| 294 | determine-if-a-string-is-numeric-1 |   |  |  |
| 295 | determine-if-a-string-is-numeric-2 |   |  |  |
| 296 | determine-if-a-string-is-squeezable |   |  |  |
| 297 | determine-if-only-one-instance-is-running |   |  |  |
| 298 | determine-if-two-triangles-overlap |   |  |  |
| 299 | determine-sentence-type |   |  |  |
| 300 | dice-game-probabilities-1 | ✓ | 240µs | 8.7 MB |
| 301 | dice-game-probabilities-2 | ✓ |  |  |
| 302 | digital-root-multiplicative-digital-root | ✓ |  |  |
| 303 | dijkstras-algorithm | error |  |  |
| 304 | dinesmans-multiple-dwelling-problem |   |  |  |
| 305 | dining-philosophers-1 |   |  |  |
| 306 | dining-philosophers-2 |   |  |  |
| 307 | disarium-numbers |   |  |  |
| 308 | discordian-date |   |  |  |
| 309 | display-a-linear-combination |   |  |  |
| 310 | display-an-outline-as-a-nested-table |   |  |  |
| 311 | distance-and-bearing |   |  |  |
| 312 | distributed-programming |   |  |  |
| 313 | diversity-prediction-theorem |   |  |  |
| 314 | documentation |   |  |  |
| 315 | doomsday-rule |   |  |  |
| 316 | dot-product |   |  |  |
| 317 | doubly-linked-list-definition-1 |   |  |  |
| 318 | doubly-linked-list-definition-2 |   |  |  |
| 319 | doubly-linked-list-element-definition |   |  |  |
| 320 | doubly-linked-list-traversal |   |  |  |
| 321 | dragon-curve |   |  |  |
| 322 | draw-a-clock |   |  |  |
| 323 | draw-a-cuboid |   |  |  |
| 324 | draw-a-pixel-1 |   |  |  |
| 325 | draw-a-rotating-cube |   |  |  |
| 326 | draw-a-sphere |   |  |  |
| 327 | dutch-national-flag-problem |   |  |  |
| 328 | dynamic-variable-names |   |  |  |
| 329 | earliest-difference-between-prime-gaps |   |  |  |
| 330 | eban-numbers |   |  |  |
| 331 | ecdsa-example |   |  |  |
| 332 | echo-server |   |  |  |
| 333 | eertree |   |  |  |
| 334 | egyptian-division |   |  |  |
| 335 | ekg-sequence-convergence |   |  |  |
| 336 | element-wise-operations |   |  |  |
| 337 | elementary-cellular-automaton-infinite-length |   |  |  |
| 338 | elementary-cellular-automaton-random-number-generator |   |  |  |
| 339 | elementary-cellular-automaton |   |  |  |
| 340 | elliptic-curve-arithmetic |   |  |  |
| 341 | elliptic-curve-digital-signature-algorithm |   |  |  |
| 342 | emirp-primes |   |  |  |
| 343 | empty-directory |   |  |  |
| 344 | empty-program |   |  |  |
| 345 | empty-string-1 |   |  |  |
| 346 | empty-string-2 |   |  |  |
| 347 | enforced-immutability |   |  |  |
| 348 | entropy-1 |   |  |  |
| 349 | entropy-2 |   |  |  |
| 350 | entropy-narcissist |   |  |  |
| 351 | enumerations-1 |   |  |  |
| 352 | enumerations-2 |   |  |  |
| 353 | enumerations-3 |   |  |  |
| 354 | enumerations-4 |   |  |  |
| 355 | environment-variables-1 |   |  |  |
| 356 | environment-variables-2 |   |  |  |
| 357 | equal-prime-and-composite-sums |   |  |  |
| 358 | equilibrium-index |   |  |  |
| 359 | erd-s-nicolas-numbers |   |  |  |
| 360 | erd-s-selfridge-categorization-of-primes |   |  |  |
| 361 | esthetic-numbers |   |  |  |
| 362 | ethiopian-multiplication |   |  |  |
| 363 | euclid-mullin-sequence |   |  |  |
| 364 | euler-method |   |  |  |
| 365 | eulers-constant-0.5772... |   |  |  |
| 366 | eulers-identity |   |  |  |
| 367 | eulers-sum-of-powers-conjecture |   |  |  |
| 368 | evaluate-binomial-coefficients |   |  |  |
| 369 | even-or-odd |   |  |  |
| 370 | events |   |  |  |
| 371 | evolutionary-algorithm |   |  |  |
| 372 | exceptions-catch-an-exception-thrown-in-a-nested-call |   |  |  |
| 373 | exceptions |   |  |  |
| 374 | executable-library |   |  |  |
| 375 | execute-a-markov-algorithm |   |  |  |
| 376 | execute-a-system-command |   |  |  |
| 377 | execute-brain- |   |  |  |
| 378 | execute-computer-zero-1 |   |  |  |
| 379 | execute-computer-zero |   |  |  |
| 380 | execute-hq9+ |   |  |  |
| 381 | execute-snusp |   |  |  |
| 382 | exponentiation-operator-2 |   |  |  |
| 383 | exponentiation-operator |   |  |  |
| 384 | exponentiation-order |   |  |  |
| 385 | exponentiation-with-infix-operators-in-or-operating-on-the-base |   |  |  |
| 386 | extend-your-language |   |  |  |
| 387 | extensible-prime-generator |   |  |  |
| 388 | extreme-floating-point-values |   |  |  |
| 389 | faces-from-a-mesh-2 |   |  |  |
| 390 | faces-from-a-mesh |   |  |  |
| 391 | factorial-base-numbers-indexing-permutations-of-a-collection |   |  |  |
| 392 | factorial-primes |   |  |  |
| 393 | factorial |   |  |  |
| 394 | factorions |   |  |  |
| 395 | factors-of-a-mersenne-number |   |  |  |
| 396 | factors-of-an-integer |   |  |  |
| 397 | fairshare-between-two-and-more |   |  |  |
| 398 | farey-sequence |   |  |  |
| 399 | fast-fourier-transform |   |  |  |
| 400 | fasta-format |   |  |  |
| 401 | faulhabers-formula |   |  |  |
| 402 | faulhabers-triangle |   |  |  |
| 403 | feigenbaum-constant-calculation |   |  |  |
| 404 | fermat-numbers |   |  |  |
| 405 | fibonacci-n-step-number-sequences |   |  |  |
| 406 | fibonacci-sequence-1 |   |  |  |
| 407 | fibonacci-sequence-2 |   |  |  |
| 408 | fibonacci-sequence-3 |   |  |  |
| 409 | fibonacci-sequence-4 |   |  |  |
| 410 | fibonacci-sequence-5 |   |  |  |
| 411 | fibonacci-word-fractal |   |  |  |
| 412 | fibonacci-word |   |  |  |
| 413 | file-extension-is-in-extensions-list |   |  |  |
| 414 | file-input-output-1 |   |  |  |
| 415 | file-input-output-2 |   |  |  |
| 416 | file-input-output-3 |   |  |  |
| 417 | file-modification-time |   |  |  |
| 418 | file-size-distribution |   |  |  |
| 419 | file-size |   |  |  |
| 420 | filter |   |  |  |
| 421 | find-chess960-starting-position-identifier-2 |   |  |  |
| 422 | find-chess960-starting-position-identifier |   |  |  |
| 423 | find-common-directory-path |   |  |  |
| 424 | find-duplicate-files |   |  |  |
| 425 | find-if-a-point-is-within-a-triangle |   |  |  |
| 426 | find-largest-left-truncatable-prime-in-a-given-base |   |  |  |
| 427 | find-limit-of-recursion |   |  |  |
| 428 | find-palindromic-numbers-in-both-binary-and-ternary-bases |   |  |  |
| 429 | find-the-intersection-of-a-line-with-a-plane |   |  |  |
| 430 | find-the-intersection-of-two-lines |   |  |  |
| 431 | find-the-last-sunday-of-each-month |   |  |  |
| 432 | find-the-missing-permutation |   |  |  |
| 433 | first-class-environments |   |  |  |
| 434 | first-class-functions-use-numbers-analogously |   |  |  |
| 435 | first-power-of-2-that-has-leading-decimal-digits-of-12 |   |  |  |
| 436 | five-weekends |   |  |  |
| 437 | fivenum-1 |   |  |  |
| 438 | fivenum-2 |   |  |  |
| 439 | fivenum-3 |   |  |  |
| 440 | fixed-length-records-1 |   |  |  |
| 441 | fixed-length-records-2 |   |  |  |
| 442 | fizzbuzz-1 |   |  |  |
| 443 | fizzbuzz-2 |   |  |  |
| 444 | fizzbuzz |   |  |  |
| 445 | flatten-a-list-1 |   |  |  |
| 446 | flatten-a-list-2 |   |  |  |
| 447 | flipping-bits-game |   |  |  |
| 448 | flow-control-structures-1 |   |  |  |
| 449 | flow-control-structures-2 |   |  |  |
| 450 | flow-control-structures-3 |   |  |  |
| 451 | flow-control-structures-4 |   |  |  |
| 452 | floyd-warshall-algorithm |   |  |  |
| 453 | floyd-warshall-algorithm2 |   |  |  |
| 454 | floyds-triangle |   |  |  |
| 455 | forest-fire |   |  |  |
| 456 | fork-2 |   |  |  |
| 457 | fork |   |  |  |
| 458 | formal-power-series |   |  |  |
| 459 | formatted-numeric-output |   |  |  |
| 460 | forward-difference |   |  |  |
| 461 | four-bit-adder-1 |   |  |  |
| 462 | four-is-magic |   |  |  |
| 463 | four-is-the-number-of-letters-in-the-... |   |  |  |
| 464 | fractal-tree |   |  |  |
| 465 | fractran |   |  |  |
| 466 | french-republican-calendar |   |  |  |
| 467 | ftp |   |  |  |
| 468 | function-frequency |   |  |  |
| 469 | function-prototype |   |  |  |
| 470 | functional-coverage-tree |   |  |  |
| 471 | fusc-sequence |   |  |  |
| 472 | gamma-function |   |  |  |
| 473 | general-fizzbuzz |   |  |  |
| 474 | generic-swap |   |  |  |
| 475 | get-system-command-output |   |  |  |
| 476 | giuga-numbers |   |  |  |
| 477 | globally-replace-text-in-several-files |   |  |  |
| 478 | goldbachs-comet |   |  |  |
| 479 | golden-ratio-convergence |   |  |  |
| 480 | graph-colouring |   |  |  |
| 481 | gray-code |   |  |  |
| 482 | gui-component-interaction |   |  |  |
| 483 | gui-enabling-disabling-of-controls |   |  |  |
| 484 | gui-maximum-window-dimensions |   |  |  |
| 485 | http |   |  |  |
| 486 | image-noise |   |  |  |
| 487 | loops-increment-loop-index-within-loop-body |   |  |  |
| 488 | md5 |   |  |  |
| 489 | nim-game |   |  |  |
| 490 | plasma-effect |   |  |  |
| 491 | sorting-algorithms-bubble-sort |   |  |  |
| 492 | window-management |   |  |  |
| 493 | zumkeller-numbers |   |  |  |
