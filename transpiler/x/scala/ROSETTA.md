# Scala Transpiler Rosetta Output

Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.

## Golden Test Checklist (177/332)
_Last updated: 2025-07-26 21:26 +0700_

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
| 17 | a+b | ✓ | 6µs | 8.7 MB |
| 18 | abbreviations-automatic | ✓ | 34µs | 8.7 MB |
| 19 | abbreviations-easy | ✓ | 14µs | 8.7 MB |
| 20 | abbreviations-simple | ✓ | 18µs | 8.7 MB |
| 21 | abc-problem | ✓ | 13µs | 8.7 MB |
| 22 | abelian-sandpile-model-identity | ✓ | 10µs | 8.7 MB |
| 23 | abelian-sandpile-model | ✓ | 7µs | 8.7 MB |
| 24 | abstract-type | ✓ | 3µs | 8.7 MB |
| 25 | abundant-deficient-and-perfect-number-classifications | ✓ | 255µs | 8.7 MB |
| 26 | abundant-odd-numbers | ✓ | 654µs | 8.7 MB |
| 27 | accumulator-factory | ✓ | 4µs | 8.7 MB |
| 28 | achilles-numbers | ✓ | 65µs | 8.7 MB |
| 29 | ackermann-function-2 | ✓ | 3µs | 8.7 MB |
| 30 | ackermann-function-3 | ✓ |  |  |
| 31 | ackermann-function | ✓ | 4µs | 8.7 MB |
| 32 | active-directory-connect | ✓ | 4µs | 8.7 MB |
| 33 | active-directory-search-for-a-user | ✓ | 4µs | 8.7 MB |
| 34 | active-object | ✓ | 39µs | 8.7 MB |
| 35 | add-a-variable-to-a-class-instance-at-runtime | ✓ | 5µs | 8.7 MB |
| 36 | additive-primes | ✓ | 6µs | 8.7 MB |
| 37 | address-of-a-variable | ✓ | 8µs | 8.7 MB |
| 38 | adfgvx-cipher | ✓ | 15µs | 8.7 MB |
| 39 | aks-test-for-primes | ✓ | 5µs | 8.7 MB |
| 40 | algebraic-data-types | ✓ | 7µs | 8.7 MB |
| 41 | align-columns | ✓ | 9µs | 8.7 MB |
| 42 | aliquot-sequence-classifications | ✓ | 20µs | 8.7 MB |
| 43 | almkvist-giullera-formula-for-pi | ✓ | 252µs | 8.7 MB |
| 44 | almost-prime | ✓ | 4µs | 8.7 MB |
| 45 | amb | ✓ | 3µs | 8.7 MB |
| 46 | amicable-pairs | ✓ | 388µs | 8.7 MB |
| 47 | anagrams-deranged-anagrams | ✓ | 7µs | 8.7 MB |
| 48 | anagrams | ✓ | 10µs | 8.7 MB |
| 49 | angle-difference-between-two-bearings-1 | ✓ | 5µs | 8.7 MB |
| 50 | angle-difference-between-two-bearings-2 | ✓ | 5µs | 8.7 MB |
| 51 | angles-geometric-normalization-and-conversion | ✓ | 9µs | 8.8 MB |
| 52 | animate-a-pendulum | ✓ | 17µs | 8.7 MB |
| 53 | animation | ✓ | 8µs | 8.7 MB |
| 54 | anonymous-recursion-1 | ✓ | 3µs | 8.7 MB |
| 55 | anonymous-recursion-2 | ✓ | 3µs | 8.7 MB |
| 56 | anonymous-recursion | ✓ | 2µs | 8.7 MB |
| 57 | anti-primes | ✓ | 31µs | 8.7 MB |
| 58 | append-a-record-to-the-end-of-a-text-file | ✓ | 2µs | 8.7 MB |
| 59 | apply-a-callback-to-an-array-1 | ✓ | 4µs | 8.7 MB |
| 60 | apply-a-callback-to-an-array-2 | ✓ | 4µs | 8.7 MB |
| 61 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 8µs | 8.7 MB |
| 62 | approximate-equality | ✓ | 5µs | 8.7 MB |
| 63 | arbitrary-precision-integers-included- | ✓ | 237µs | 105.4 KB |
| 64 | archimedean-spiral | ✓ | 261µs | 8.7 MB |
| 65 | arena-storage-pool | ✓ | 7µs | 8.7 MB |
| 66 | arithmetic-complex | ✓ | 5µs | 8.7 MB |
| 67 | arithmetic-derivative | ✓ | 14µs | 8.7 MB |
| 68 | arithmetic-evaluation | ✓ | 4µs | 8.7 MB |
| 69 | arithmetic-geometric-mean-calculate-pi | ✓ | 4µs | 8.7 MB |
| 70 | arithmetic-geometric-mean | ✓ | 3µs | 8.7 MB |
| 71 | arithmetic-integer-1 | ✓ | 2µs | 8.7 MB |
| 72 | arithmetic-integer-2 | ✓ | 2µs | 8.7 MB |
| 73 | arithmetic-numbers | ✓ |  |  |
| 74 | arithmetic-rational | ✓ | 6µs | 8.7 MB |
| 75 | array-concatenation | ✓ | 4µs | 8.7 MB |
| 76 | array-length | ✓ | 2µs | 8.7 MB |
| 77 | arrays | ✓ | 5µs | 8.7 MB |
| 78 | ascending-primes | ✓ | 7µs | 8.7 MB |
| 79 | ascii-art-diagram-converter | ✓ | 3µs | 8.7 MB |
| 80 | assertions | ✓ | 2µs | 8.7 MB |
| 81 | associative-array-creation | ✓ | 7µs | 8.7 MB |
| 82 | associative-array-iteration | ✓ | 4µs | 3.5 MB |
| 83 | associative-array-merging | ✓ | 5µs | 3.5 MB |
| 84 | atomic-updates | ✓ | 10µs | 3.5 MB |
| 85 | attractive-numbers | ✓ | 1µs | 3.5 MB |
| 86 | average-loop-length | ✓ | 170µs | 3.5 MB |
| 87 | averages-arithmetic-mean | ✓ | 9µs | 3.5 MB |
| 88 | averages-mean-time-of-day | ✓ | 24µs | 3.5 MB |
| 89 | averages-median-1 | ✓ | 4µs | 3.5 MB |
| 90 | averages-median-2 | ✓ | 4µs | 3.5 MB |
| 91 | averages-median-3 | ✓ | 8µs | 3.5 MB |
| 92 | averages-mode | ✓ | 10µs | 3.5 MB |
| 93 | averages-pythagorean-means | ✓ | 10µs | 3.5 MB |
| 94 | averages-root-mean-square | ✓ | 1µs | 3.5 MB |
| 95 | averages-simple-moving-average | ✓ | 6µs | 3.5 MB |
| 96 | avl-tree | ✓ | 10µs | 3.5 MB |
| 97 | b-zier-curves-intersections | ✓ | 56µs | 3.5 MB |
| 98 | babbage-problem | ✓ | 4µs | 3.5 MB |
| 99 | babylonian-spiral | ✓ | 19µs | 3.5 MB |
| 100 | balanced-brackets | ✓ | 8µs | 3.5 MB |
| 101 | balanced-ternary | ✓ | 5µs | 3.5 MB |
| 102 | barnsley-fern | ✓ | 31µs | 3.5 MB |
| 103 | base64-decode-data | ✓ | 9µs | 3.5 MB |
| 104 | bell-numbers | ✓ | 17µs | 3.5 MB |
| 105 | benfords-law | ✓ | 29µs | 3.5 MB |
| 106 | bernoulli-numbers | ✓ | 92µs | 3.5 MB |
| 107 | best-shuffle | ✓ | 6µs | 3.5 MB |
| 108 | bifid-cipher | ✓ | 14µs | 3.5 MB |
| 109 | bin-given-limits | ✓ | 10µs | 3.5 MB |
| 110 | binary-digits | ✓ | 2µs | 3.5 MB |
| 111 | binary-search | ✓ | 5µs | 3.5 MB |
| 112 | binary-strings |   |  |  |
| 113 | bioinformatics-base-count | ✓ | 4µs | 8.7 MB |
| 114 | bioinformatics-global-alignment | ✓ | 199µs | 8.7 MB |
| 115 | bioinformatics-sequence-mutation | ✓ | 21µs | 8.7 MB |
| 116 | biorhythms | ✓ | 11µs | 8.7 MB |
| 117 | bitcoin-address-validation | ✓ | 25µs | 8.9 MB |
| 118 | bitmap-b-zier-curves-cubic | error |  |  |
| 119 | bitmap-b-zier-curves-quadratic | error |  |  |
| 120 | bitmap-bresenhams-line-algorithm | ✓ | 7µs | 8.7 MB |
| 121 | bitmap-flood-fill | ✓ | 4µs | 8.7 MB |
| 122 | bitmap-histogram | ✓ | 5µs | 8.7 MB |
| 123 | bitmap-midpoint-circle-algorithm | ✓ | 28µs | 8.7 MB |
| 124 | bitmap-ppm-conversion-through-a-pipe | ✓ | 81µs | 8.7 MB |
| 125 | bitmap-read-a-ppm-file | error |  |  |
| 126 | bitmap-read-an-image-through-a-pipe | error |  |  |
| 127 | bitmap-write-a-ppm-file | ✓ | 6µs | 8.7 MB |
| 128 | bitmap | error |  |  |
| 129 | bitwise-io-1 | ✓ | 3µs | 8.7 MB |
| 130 | bitwise-io-2 | error |  |  |
| 131 | bitwise-operations | ✓ | 6µs | 8.7 MB |
| 132 | blum-integer | ✓ | 7µs | 8.7 MB |
| 133 | boolean-values | ✓ | 6µs | 8.7 MB |
| 134 | box-the-compass | ✓ | 7µs | 8.7 MB |
| 135 | boyer-moore-string-search | ✓ | 39µs | 8.7 MB |
| 136 | brazilian-numbers | ✓ | 2.52ms | 8.7 MB |
| 137 | break-oo-privacy | ✓ | 3µs | 8.7 MB |
| 138 | brilliant-numbers | error |  |  |
| 139 | brownian-tree | ✓ |  |  |
| 140 | bulls-and-cows-player | ✓ | 39µs | 8.7 MB |
| 141 | bulls-and-cows | error |  |  |
| 142 | burrows-wheeler-transform | ✓ | 25µs | 8.7 MB |
| 143 | caesar-cipher-1 | ✓ | 8µs | 8.7 MB |
| 144 | caesar-cipher-2 | ✓ | 11µs | 8.7 MB |
| 145 | calculating-the-value-of-e | error |  |  |
| 146 | calendar---for-real-programmers-1 | ✓ | 56µs | 8.7 MB |
| 147 | calendar---for-real-programmers-2 | ✓ | 47µs | 8.7 MB |
| 148 | calendar | ✓ | 54µs | 8.7 MB |
| 149 | calkin-wilf-sequence | error |  |  |
| 150 | call-a-foreign-language-function | ✓ | 2µs | 8.7 MB |
| 151 | call-a-function-1 | ✓ |  | 8.7 MB |
| 152 | call-a-function-10 | ✓ | 1µs | 8.7 MB |
| 153 | call-a-function-11 | ✓ | 3µs | 8.7 MB |
| 154 | call-a-function-12 | ✓ | 3µs | 8.7 MB |
| 155 | call-a-function-2 | ✓ | 3µs | 8.7 MB |
| 156 | call-a-function-3 | ✓ | 7µs | 8.7 MB |
| 157 | call-a-function-4 | ✓ | 1µs | 8.7 MB |
| 158 | call-a-function-5 | ✓ | 3µs | 8.7 MB |
| 159 | call-a-function-6 | ✓ | 9µs | 8.7 MB |
| 160 | call-a-function-7 | ✓ |  | 8.7 MB |
| 161 | call-a-function-8 | error |  |  |
| 162 | call-a-function-9 | ✓ | 3µs | 8.7 MB |
| 163 | call-an-object-method-1 | ✓ | 1µs | 8.7 MB |
| 164 | call-an-object-method-2 | ✓ | 2µs | 8.7 MB |
| 165 | call-an-object-method-3 | ✓ | 1µs | 8.7 MB |
| 166 | call-an-object-method | ✓ | 1µs | 8.7 MB |
| 167 | camel-case-and-snake-case | error |  |  |
| 168 | canny-edge-detector | error |  |  |
| 169 | canonicalize-cidr | error |  |  |
| 170 | cantor-set | error |  |  |
| 171 | carmichael-3-strong-pseudoprimes | error |  |  |
| 172 | cartesian-product-of-two-or-more-lists-1 | ✓ | 4µs | 8.7 MB |
| 173 | cartesian-product-of-two-or-more-lists-2 | ✓ | 12µs | 8.7 MB |
| 174 | cartesian-product-of-two-or-more-lists-3 | ✓ | 11µs | 8.7 MB |
| 175 | cartesian-product-of-two-or-more-lists-4 | error |  |  |
| 176 | case-sensitivity-of-identifiers | ✓ | 5µs | 8.7 MB |
| 177 | casting-out-nines | error |  |  |
| 178 | catalan-numbers-1 | ✓ | 17µs | 8.7 MB |
| 179 | catalan-numbers-2 | ✓ | 20µs | 8.7 MB |
| 180 | catalan-numbers-pascals-triangle | error |  |  |
| 181 | catamorphism | ✓ | 4µs | 8.7 MB |
| 182 | catmull-clark-subdivision-surface | error |  |  |
| 183 | chaocipher | ✓ | 7µs | 8.7 MB |
| 184 | chaos-game | error |  |  |
| 185 | character-codes-1 | ✓ | 3µs | 8.7 MB |
| 186 | character-codes-2 | ✓ | 3µs | 8.7 MB |
| 187 | character-codes-3 | ✓ | 17µs | 8.7 MB |
| 188 | character-codes-4 | ✓ | 3µs | 8.7 MB |
| 189 | character-codes-5 | ✓ | 3µs | 8.7 MB |
| 190 | chat-server | ✓ | 3µs | 8.7 MB |
| 191 | check-machin-like-formulas | ✓ | 32µs | 8.7 MB |
| 192 | check-that-file-exists | ✓ | 4µs | 8.7 MB |
| 193 | checkpoint-synchronization-1 | ✓ | 17µs | 8.7 MB |
| 194 | checkpoint-synchronization-2 | ✓ | 16µs | 8.7 MB |
| 195 | checkpoint-synchronization-3 | ✓ | 17µs | 8.7 MB |
| 196 | checkpoint-synchronization-4 | ✓ | 20µs | 8.7 MB |
| 197 | chernicks-carmichael-numbers | ✓ | 2.23ms | 8.7 MB |
| 198 | cheryls-birthday | ✓ | 5µs | 8.7 MB |
| 199 | chinese-remainder-theorem | ✓ | 22µs | 8.7 MB |
| 200 | chinese-zodiac | ✓ | 4µs | 8.7 MB |
| 201 | cholesky-decomposition-1 | error |  |  |
| 202 | cholesky-decomposition |   |  |  |
| 203 | chowla-numbers |   |  |  |
| 204 | church-numerals-1 |   |  |  |
| 205 | church-numerals-2 |   |  |  |
| 206 | circles-of-given-radius-through-two-points |   |  |  |
| 207 | circular-primes |   |  |  |
| 208 | cistercian-numerals |   |  |  |
| 209 | comma-quibbling |   |  |  |
| 210 | compiler-virtual-machine-interpreter |   |  |  |
| 211 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k |   |  |  |
| 212 | compound-data-type |   |  |  |
| 213 | concurrent-computing-1 |   |  |  |
| 214 | concurrent-computing-2 |   |  |  |
| 215 | concurrent-computing-3 |   |  |  |
| 216 | conditional-structures-1 |   |  |  |
| 217 | conditional-structures-10 |   |  |  |
| 218 | conditional-structures-2 |   |  |  |
| 219 | conditional-structures-3 |   |  |  |
| 220 | conditional-structures-4 |   |  |  |
| 221 | conditional-structures-5 |   |  |  |
| 222 | conditional-structures-6 |   |  |  |
| 223 | conditional-structures-7 |   |  |  |
| 224 | conditional-structures-8 |   |  |  |
| 225 | conditional-structures-9 |   |  |  |
| 226 | consecutive-primes-with-ascending-or-descending-differences |   |  |  |
| 227 | constrained-genericity-1 |   |  |  |
| 228 | constrained-genericity-2 |   |  |  |
| 229 | constrained-genericity-3 |   |  |  |
| 230 | constrained-genericity-4 |   |  |  |
| 231 | constrained-random-points-on-a-circle-1 |   |  |  |
| 232 | constrained-random-points-on-a-circle-2 |   |  |  |
| 233 | continued-fraction |   |  |  |
| 234 | convert-decimal-number-to-rational |   |  |  |
| 235 | convert-seconds-to-compound-duration |   |  |  |
| 236 | convex-hull |   |  |  |
| 237 | conways-game-of-life |   |  |  |
| 238 | copy-a-string-1 |   |  |  |
| 239 | copy-a-string-2 |   |  |  |
| 240 | copy-stdin-to-stdout-1 |   |  |  |
| 241 | copy-stdin-to-stdout-2 |   |  |  |
| 242 | count-in-factors |   |  |  |
| 243 | count-in-octal-1 |   |  |  |
| 244 | count-in-octal-2 |   |  |  |
| 245 | count-in-octal-3 |   |  |  |
| 246 | count-in-octal-4 |   |  |  |
| 247 | count-occurrences-of-a-substring |   |  |  |
| 248 | count-the-coins-1 |   |  |  |
| 249 | count-the-coins-2 |   |  |  |
| 250 | cramers-rule |   |  |  |
| 251 | crc-32-1 |   |  |  |
| 252 | crc-32-2 |   |  |  |
| 253 | create-a-file-on-magnetic-tape |   |  |  |
| 254 | create-a-file |   |  |  |
| 255 | create-a-two-dimensional-array-at-runtime-1 |   |  |  |
| 256 | create-an-html-table |   |  |  |
| 257 | create-an-object-at-a-given-address |   |  |  |
| 258 | csv-data-manipulation |   |  |  |
| 259 | csv-to-html-translation-1 |   |  |  |
| 260 | csv-to-html-translation-2 |   |  |  |
| 261 | csv-to-html-translation-3 |   |  |  |
| 262 | csv-to-html-translation-4 |   |  |  |
| 263 | csv-to-html-translation-5 |   |  |  |
| 264 | cuban-primes |   |  |  |
| 265 | cullen-and-woodall-numbers |   |  |  |
| 266 | cumulative-standard-deviation |   |  |  |
| 267 | currency |   |  |  |
| 268 | currying |   |  |  |
| 269 | curzon-numbers |   |  |  |
| 270 | cusip |   |  |  |
| 271 | cyclops-numbers |   |  |  |
| 272 | damm-algorithm |   |  |  |
| 273 | date-format |   |  |  |
| 274 | date-manipulation |   |  |  |
| 275 | day-of-the-week |   |  |  |
| 276 | de-bruijn-sequences |   |  |  |
| 277 | deal-cards-for-freecell |   |  |  |
| 278 | death-star |   |  |  |
| 279 | deceptive-numbers |   |  |  |
| 280 | deconvolution-1d-2 |   |  |  |
| 281 | deconvolution-1d-3 |   |  |  |
| 282 | deconvolution-1d |   |  |  |
| 283 | deepcopy-1 |   |  |  |
| 284 | define-a-primitive-data-type |   |  |  |
| 285 | delegates |   |  |  |
| 286 | demings-funnel |   |  |  |
| 287 | department-numbers |   |  |  |
| 288 | descending-primes |   |  |  |
| 289 | detect-division-by-zero |   |  |  |
| 290 | determine-if-a-string-has-all-the-same-characters |   |  |  |
| 291 | determine-if-a-string-has-all-unique-characters |   |  |  |
| 292 | determine-if-a-string-is-collapsible |   |  |  |
| 293 | determine-if-a-string-is-numeric-1 |   |  |  |
| 294 | determine-if-a-string-is-numeric-2 |   |  |  |
| 295 | determine-if-a-string-is-squeezable |   |  |  |
| 296 | determine-if-only-one-instance-is-running |   |  |  |
| 297 | determine-if-two-triangles-overlap |   |  |  |
| 298 | determine-sentence-type |   |  |  |
| 299 | dice-game-probabilities-1 |   |  |  |
| 300 | dice-game-probabilities-2 |   |  |  |
| 301 | digital-root-multiplicative-digital-root |   |  |  |
| 302 | dijkstras-algorithm |   |  |  |
| 303 | dinesmans-multiple-dwelling-problem |   |  |  |
| 304 | dining-philosophers-1 |   |  |  |
| 305 | dining-philosophers-2 |   |  |  |
| 306 | disarium-numbers |   |  |  |
| 307 | discordian-date |   |  |  |
| 308 | display-a-linear-combination |   |  |  |
| 309 | display-an-outline-as-a-nested-table |   |  |  |
| 310 | distance-and-bearing |   |  |  |
| 311 | distributed-programming |   |  |  |
| 312 | diversity-prediction-theorem |   |  |  |
| 313 | documentation |   |  |  |
| 314 | doomsday-rule |   |  |  |
| 315 | dot-product |   |  |  |
| 316 | doubly-linked-list-definition-1 |   |  |  |
| 317 | doubly-linked-list-definition-2 |   |  |  |
| 318 | doubly-linked-list-element-definition |   |  |  |
| 319 | doubly-linked-list-traversal |   |  |  |
| 320 | dragon-curve |   |  |  |
| 321 | draw-a-clock |   |  |  |
| 322 | draw-a-cuboid |   |  |  |
| 323 | draw-a-pixel-1 |   |  |  |
| 324 | draw-a-rotating-cube |   |  |  |
| 325 | draw-a-sphere |   |  |  |
| 326 | dutch-national-flag-problem |   |  |  |
| 327 | dynamic-variable-names |   |  |  |
| 328 | earliest-difference-between-prime-gaps |   |  |  |
| 329 | eban-numbers |   |  |  |
| 330 | echo-server |   |  |  |
| 331 | ekg-sequence-convergence |   |  |  |
| 332 | md5 |   |  |  |
