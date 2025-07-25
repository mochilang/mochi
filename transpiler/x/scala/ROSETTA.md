# Scala Transpiler Rosetta Output

Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.

## Golden Test Checklist (134/284)
_Last updated: 2025-07-26 05:17 +0700_

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
| 16 | a+b | ✓ | 6µs | 8.7 MB |
| 17 | abbreviations-automatic | ✓ | 34µs | 8.7 MB |
| 18 | abbreviations-easy | ✓ | 14µs | 8.7 MB |
| 19 | abbreviations-simple | ✓ | 18µs | 8.7 MB |
| 20 | abc-problem | ✓ | 13µs | 8.7 MB |
| 21 | abelian-sandpile-model-identity | ✓ | 10µs | 8.7 MB |
| 22 | abelian-sandpile-model | ✓ | 7µs | 8.7 MB |
| 23 | abstract-type | ✓ | 3µs | 8.7 MB |
| 24 | abundant-deficient-and-perfect-number-classifications | ✓ | 255µs | 8.7 MB |
| 25 | abundant-odd-numbers | ✓ | 654µs | 8.7 MB |
| 26 | accumulator-factory | ✓ | 4µs | 8.7 MB |
| 27 | achilles-numbers | ✓ | 65µs | 8.7 MB |
| 28 | ackermann-function-2 | ✓ | 3µs | 8.7 MB |
| 29 | ackermann-function-3 | ✓ |  |  |
| 30 | ackermann-function | ✓ | 4µs | 8.7 MB |
| 31 | active-directory-connect | ✓ | 4µs | 8.7 MB |
| 32 | active-directory-search-for-a-user | ✓ | 4µs | 8.7 MB |
| 33 | active-object | ✓ | 39µs | 8.7 MB |
| 34 | add-a-variable-to-a-class-instance-at-runtime | ✓ | 5µs | 8.7 MB |
| 35 | additive-primes | ✓ | 6µs | 8.7 MB |
| 36 | address-of-a-variable | ✓ | 8µs | 8.7 MB |
| 37 | adfgvx-cipher | ✓ | 15µs | 8.7 MB |
| 38 | aks-test-for-primes | ✓ | 5µs | 8.7 MB |
| 39 | algebraic-data-types | ✓ | 7µs | 8.7 MB |
| 40 | align-columns | ✓ | 9µs | 8.7 MB |
| 41 | aliquot-sequence-classifications | ✓ | 20µs | 8.7 MB |
| 42 | almkvist-giullera-formula-for-pi | ✓ | 252µs | 8.7 MB |
| 43 | almost-prime | ✓ | 4µs | 8.7 MB |
| 44 | amb | ✓ | 3µs | 8.7 MB |
| 45 | amicable-pairs | ✓ | 388µs | 8.7 MB |
| 46 | anagrams-deranged-anagrams | ✓ | 7µs | 8.7 MB |
| 47 | anagrams | ✓ | 10µs | 8.7 MB |
| 48 | angle-difference-between-two-bearings-1 | ✓ | 5µs | 8.7 MB |
| 49 | angle-difference-between-two-bearings-2 | ✓ | 5µs | 8.7 MB |
| 50 | angles-geometric-normalization-and-conversion | ✓ | 9µs | 8.8 MB |
| 51 | animate-a-pendulum | ✓ | 17µs | 8.7 MB |
| 52 | animation | ✓ | 8µs | 8.7 MB |
| 53 | anonymous-recursion-1 | ✓ | 3µs | 8.7 MB |
| 54 | anonymous-recursion-2 | ✓ | 3µs | 8.7 MB |
| 55 | anonymous-recursion | ✓ | 2µs | 8.7 MB |
| 56 | anti-primes | ✓ | 31µs | 8.7 MB |
| 57 | append-a-record-to-the-end-of-a-text-file | ✓ | 2µs | 8.7 MB |
| 58 | apply-a-callback-to-an-array-1 | ✓ | 4µs | 8.7 MB |
| 59 | apply-a-callback-to-an-array-2 | ✓ | 4µs | 8.7 MB |
| 60 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 8µs | 8.7 MB |
| 61 | approximate-equality | ✓ | 5µs | 8.7 MB |
| 62 | arbitrary-precision-integers-included- | ✓ | 237µs | 105.4 KB |
| 63 | archimedean-spiral | ✓ | 261µs | 8.7 MB |
| 64 | arena-storage-pool | ✓ | 7µs | 8.7 MB |
| 65 | arithmetic-complex | ✓ | 5µs | 8.7 MB |
| 66 | arithmetic-derivative | ✓ | 14µs | 8.7 MB |
| 67 | arithmetic-evaluation | ✓ | 4µs | 8.7 MB |
| 68 | arithmetic-geometric-mean-calculate-pi | ✓ | 4µs | 8.7 MB |
| 69 | arithmetic-geometric-mean | ✓ | 3µs | 8.7 MB |
| 70 | arithmetic-integer-1 | ✓ | 2µs | 8.7 MB |
| 71 | arithmetic-integer-2 | ✓ | 2µs | 8.7 MB |
| 72 | arithmetic-numbers | ✓ |  |  |
| 73 | arithmetic-rational | ✓ | 6µs | 8.7 MB |
| 74 | array-concatenation | ✓ | 4µs | 8.7 MB |
| 75 | array-length | ✓ | 2µs | 8.7 MB |
| 76 | arrays | ✓ | 5µs | 8.7 MB |
| 77 | ascending-primes | ✓ | 7µs | 8.7 MB |
| 78 | ascii-art-diagram-converter | ✓ | 3µs | 8.7 MB |
| 79 | assertions | ✓ | 2µs | 8.7 MB |
| 80 | associative-array-creation | ✓ | 7µs | 8.7 MB |
| 81 | associative-array-iteration | ✓ | 4µs | 3.5 MB |
| 82 | associative-array-merging | ✓ | 5µs | 3.5 MB |
| 83 | atomic-updates | ✓ | 10µs | 3.5 MB |
| 84 | attractive-numbers | ✓ | 1µs | 3.5 MB |
| 85 | average-loop-length | ✓ | 170µs | 3.5 MB |
| 86 | averages-arithmetic-mean | ✓ | 9µs | 3.5 MB |
| 87 | averages-mean-time-of-day | ✓ | 24µs | 3.5 MB |
| 88 | averages-median-1 | ✓ | 4µs | 3.5 MB |
| 89 | averages-median-2 | ✓ | 4µs | 3.5 MB |
| 90 | averages-median-3 | ✓ | 8µs | 3.5 MB |
| 91 | averages-mode | ✓ | 10µs | 3.5 MB |
| 92 | averages-pythagorean-means | ✓ | 10µs | 3.5 MB |
| 93 | averages-root-mean-square | ✓ | 1µs | 3.5 MB |
| 94 | averages-simple-moving-average | ✓ | 6µs | 3.5 MB |
| 95 | avl-tree | ✓ | 10µs | 3.5 MB |
| 96 | b-zier-curves-intersections | ✓ | 56µs | 3.5 MB |
| 97 | babbage-problem | ✓ | 4µs | 3.5 MB |
| 98 | babylonian-spiral | ✓ | 19µs | 3.5 MB |
| 99 | balanced-brackets | ✓ | 8µs | 3.5 MB |
| 100 | balanced-ternary | ✓ | 5µs | 3.5 MB |
| 101 | barnsley-fern | ✓ | 31µs | 3.5 MB |
| 102 | base64-decode-data | ✓ | 9µs | 3.5 MB |
| 103 | bell-numbers | ✓ | 17µs | 3.5 MB |
| 104 | benfords-law | ✓ | 29µs | 3.5 MB |
| 105 | bernoulli-numbers | ✓ | 92µs | 3.5 MB |
| 106 | best-shuffle | ✓ | 6µs | 3.5 MB |
| 107 | bifid-cipher | ✓ | 14µs | 3.5 MB |
| 108 | bin-given-limits | ✓ | 10µs | 3.5 MB |
| 109 | binary-digits | ✓ | 2µs | 3.5 MB |
| 110 | binary-search | ✓ | 5µs | 3.5 MB |
| 111 | binary-strings |   |  |  |
| 112 | bioinformatics-base-count | ✓ | 4µs | 8.7 MB |
| 113 | bioinformatics-global-alignment | ✓ | 199µs | 8.7 MB |
| 114 | bioinformatics-sequence-mutation | ✓ | 21µs | 8.7 MB |
| 115 | biorhythms | ✓ | 11µs | 8.7 MB |
| 116 | bitcoin-address-validation | ✓ | 25µs | 8.9 MB |
| 117 | bitmap-b-zier-curves-cubic | error |  |  |
| 118 | bitmap-b-zier-curves-quadratic | error |  |  |
| 119 | bitmap-bresenhams-line-algorithm | ✓ | 7µs | 8.7 MB |
| 120 | bitmap-flood-fill | ✓ | 4µs | 8.7 MB |
| 121 | bitmap-histogram | ✓ | 5µs | 8.7 MB |
| 122 | bitmap-midpoint-circle-algorithm | ✓ | 28µs | 8.7 MB |
| 123 | bitmap-ppm-conversion-through-a-pipe | ✓ | 81µs | 8.7 MB |
| 124 | bitmap-read-a-ppm-file | error |  |  |
| 125 | bitmap-read-an-image-through-a-pipe | error |  |  |
| 126 | bitmap-write-a-ppm-file | ✓ | 6µs | 8.7 MB |
| 127 | bitmap | error |  |  |
| 128 | bitwise-io-1 | ✓ | 3µs | 8.7 MB |
| 129 | bitwise-io-2 | error |  |  |
| 130 | bitwise-operations | ✓ | 6µs | 8.7 MB |
| 131 | blum-integer | ✓ | 7µs | 8.7 MB |
| 132 | boolean-values | ✓ | 6µs | 8.7 MB |
| 133 | box-the-compass | ✓ | 7µs | 8.7 MB |
| 134 | boyer-moore-string-search | ✓ | 39µs | 8.7 MB |
| 135 | brazilian-numbers | ✓ | 2.52ms | 8.7 MB |
| 136 | break-oo-privacy | ✓ | 3µs | 8.7 MB |
| 137 | brilliant-numbers | error |  |  |
| 138 | brownian-tree | ✓ |  |  |
| 139 | bulls-and-cows-player | ✓ | 39µs | 8.7 MB |
| 140 | bulls-and-cows | error |  |  |
| 141 | burrows-wheeler-transform | ✓ | 25µs | 8.7 MB |
| 142 | caesar-cipher-1 | ✓ | 8µs | 8.7 MB |
| 143 | caesar-cipher-2 | ✓ | 11µs | 8.7 MB |
| 144 | calculating-the-value-of-e | error |  |  |
| 145 | calendar---for-real-programmers-1 |   |  |  |
| 146 | calendar---for-real-programmers-2 |   |  |  |
| 147 | calendar |   |  |  |
| 148 | calkin-wilf-sequence |   |  |  |
| 149 | call-a-foreign-language-function |   |  |  |
| 150 | call-a-function-1 |   |  |  |
| 151 | call-a-function-10 |   |  |  |
| 152 | call-a-function-11 |   |  |  |
| 153 | call-a-function-12 |   |  |  |
| 154 | call-a-function-2 |   |  |  |
| 155 | call-a-function-3 |   |  |  |
| 156 | call-a-function-4 |   |  |  |
| 157 | call-a-function-5 |   |  |  |
| 158 | call-a-function-6 |   |  |  |
| 159 | call-a-function-7 |   |  |  |
| 160 | call-a-function-8 |   |  |  |
| 161 | call-a-function-9 |   |  |  |
| 162 | call-an-object-method-1 |   |  |  |
| 163 | call-an-object-method-2 |   |  |  |
| 164 | call-an-object-method-3 |   |  |  |
| 165 | call-an-object-method |   |  |  |
| 166 | camel-case-and-snake-case |   |  |  |
| 167 | canny-edge-detector |   |  |  |
| 168 | canonicalize-cidr |   |  |  |
| 169 | cantor-set |   |  |  |
| 170 | carmichael-3-strong-pseudoprimes |   |  |  |
| 171 | cartesian-product-of-two-or-more-lists-1 |   |  |  |
| 172 | cartesian-product-of-two-or-more-lists-2 |   |  |  |
| 173 | cartesian-product-of-two-or-more-lists-3 |   |  |  |
| 174 | cartesian-product-of-two-or-more-lists-4 |   |  |  |
| 175 | case-sensitivity-of-identifiers |   |  |  |
| 176 | casting-out-nines |   |  |  |
| 177 | catalan-numbers-1 |   |  |  |
| 178 | catalan-numbers-2 |   |  |  |
| 179 | catalan-numbers-pascals-triangle |   |  |  |
| 180 | catamorphism |   |  |  |
| 181 | catmull-clark-subdivision-surface |   |  |  |
| 182 | chaocipher |   |  |  |
| 183 | chaos-game |   |  |  |
| 184 | character-codes-1 |   |  |  |
| 185 | character-codes-2 |   |  |  |
| 186 | character-codes-3 |   |  |  |
| 187 | character-codes-4 |   |  |  |
| 188 | character-codes-5 |   |  |  |
| 189 | chat-server |   |  |  |
| 190 | check-machin-like-formulas |   |  |  |
| 191 | check-that-file-exists |   |  |  |
| 192 | checkpoint-synchronization-1 |   |  |  |
| 193 | checkpoint-synchronization-2 |   |  |  |
| 194 | checkpoint-synchronization-3 |   |  |  |
| 195 | checkpoint-synchronization-4 |   |  |  |
| 196 | chernicks-carmichael-numbers |   |  |  |
| 197 | cheryls-birthday |   |  |  |
| 198 | chinese-remainder-theorem |   |  |  |
| 199 | chinese-zodiac |   |  |  |
| 200 | cholesky-decomposition-1 |   |  |  |
| 201 | cholesky-decomposition |   |  |  |
| 202 | chowla-numbers |   |  |  |
| 203 | church-numerals-1 |   |  |  |
| 204 | church-numerals-2 |   |  |  |
| 205 | circles-of-given-radius-through-two-points |   |  |  |
| 206 | circular-primes |   |  |  |
| 207 | cistercian-numerals |   |  |  |
| 208 | comma-quibbling |   |  |  |
| 209 | compiler-virtual-machine-interpreter |   |  |  |
| 210 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k |   |  |  |
| 211 | compound-data-type |   |  |  |
| 212 | concurrent-computing-1 |   |  |  |
| 213 | concurrent-computing-2 |   |  |  |
| 214 | concurrent-computing-3 |   |  |  |
| 215 | conditional-structures-1 |   |  |  |
| 216 | conditional-structures-10 |   |  |  |
| 217 | conditional-structures-2 |   |  |  |
| 218 | conditional-structures-3 |   |  |  |
| 219 | conditional-structures-4 |   |  |  |
| 220 | conditional-structures-5 |   |  |  |
| 221 | conditional-structures-6 |   |  |  |
| 222 | conditional-structures-7 |   |  |  |
| 223 | conditional-structures-8 |   |  |  |
| 224 | conditional-structures-9 |   |  |  |
| 225 | consecutive-primes-with-ascending-or-descending-differences |   |  |  |
| 226 | constrained-genericity-1 |   |  |  |
| 227 | constrained-genericity-2 |   |  |  |
| 228 | constrained-genericity-3 |   |  |  |
| 229 | constrained-genericity-4 |   |  |  |
| 230 | constrained-random-points-on-a-circle-1 |   |  |  |
| 231 | constrained-random-points-on-a-circle-2 |   |  |  |
| 232 | continued-fraction |   |  |  |
| 233 | convert-decimal-number-to-rational |   |  |  |
| 234 | convert-seconds-to-compound-duration |   |  |  |
| 235 | convex-hull |   |  |  |
| 236 | conways-game-of-life |   |  |  |
| 237 | copy-a-string-1 |   |  |  |
| 238 | copy-a-string-2 |   |  |  |
| 239 | copy-stdin-to-stdout-1 |   |  |  |
| 240 | copy-stdin-to-stdout-2 |   |  |  |
| 241 | count-in-factors |   |  |  |
| 242 | count-in-octal-1 |   |  |  |
| 243 | count-in-octal-2 |   |  |  |
| 244 | count-in-octal-3 |   |  |  |
| 245 | count-in-octal-4 |   |  |  |
| 246 | count-occurrences-of-a-substring |   |  |  |
| 247 | count-the-coins-1 |   |  |  |
| 248 | count-the-coins-2 |   |  |  |
| 249 | cramers-rule |   |  |  |
| 250 | crc-32-1 |   |  |  |
| 251 | crc-32-2 |   |  |  |
| 252 | create-a-file-on-magnetic-tape |   |  |  |
| 253 | create-a-file |   |  |  |
| 254 | create-a-two-dimensional-array-at-runtime-1 |   |  |  |
| 255 | create-an-html-table |   |  |  |
| 256 | create-an-object-at-a-given-address |   |  |  |
| 257 | csv-data-manipulation |   |  |  |
| 258 | csv-to-html-translation-1 |   |  |  |
| 259 | csv-to-html-translation-2 |   |  |  |
| 260 | csv-to-html-translation-3 |   |  |  |
| 261 | csv-to-html-translation-4 |   |  |  |
| 262 | csv-to-html-translation-5 |   |  |  |
| 263 | cuban-primes |   |  |  |
| 264 | cullen-and-woodall-numbers |   |  |  |
| 265 | cumulative-standard-deviation |   |  |  |
| 266 | currency |   |  |  |
| 267 | currying |   |  |  |
| 268 | curzon-numbers |   |  |  |
| 269 | cusip |   |  |  |
| 270 | cyclops-numbers |   |  |  |
| 271 | damm-algorithm |   |  |  |
| 272 | date-format |   |  |  |
| 273 | date-manipulation |   |  |  |
| 274 | day-of-the-week |   |  |  |
| 275 | de-bruijn-sequences |   |  |  |
| 276 | deal-cards-for-freecell |   |  |  |
| 277 | death-star |   |  |  |
| 278 | deceptive-numbers |   |  |  |
| 279 | deconvolution-1d-2 |   |  |  |
| 280 | deconvolution-1d-3 |   |  |  |
| 281 | deconvolution-1d |   |  |  |
| 282 | deepcopy-1 |   |  |  |
| 283 | define-a-primitive-data-type |   |  |  |
| 284 | md5 |   |  |  |
