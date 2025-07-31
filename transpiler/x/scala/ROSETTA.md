# Scala Transpiler Rosetta Output

Generated Scala code for Rosetta tasks in `tests/rosetta/x/Mochi`. Each program has a `.scala` file produced by the transpiler and a `.out` file with its runtime output. Compilation or execution errors are captured in `.error` files.

## Golden Test Checklist (311/491)
_Last updated: 2025-07-31 08:01 +0700_

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
| 15 | a+b | ✓ | 6µs | 8.7 MB |
| 16 | abbreviations-automatic | ✓ | 34µs | 8.7 MB |
| 17 | abbreviations-easy | ✓ | 14µs | 8.7 MB |
| 18 | abbreviations-simple | ✓ | 18µs | 8.7 MB |
| 19 | abc-problem | ✓ | 13µs | 8.7 MB |
| 20 | abelian-sandpile-model-identity | ✓ | 10µs | 8.7 MB |
| 21 | abelian-sandpile-model | ✓ | 7µs | 8.7 MB |
| 22 | abstract-type | ✓ | 3µs | 8.7 MB |
| 23 | abundant-deficient-and-perfect-number-classifications | ✓ | 255µs | 8.7 MB |
| 24 | abundant-odd-numbers | ✓ | 654µs | 8.7 MB |
| 25 | accumulator-factory | ✓ | 4µs | 8.7 MB |
| 26 | achilles-numbers | ✓ | 65µs | 8.7 MB |
| 27 | ackermann-function-2 | ✓ | 3µs | 8.7 MB |
| 28 | ackermann-function-3 | ✓ |  |  |
| 29 | ackermann-function | ✓ | 4µs | 8.7 MB |
| 30 | active-directory-connect | ✓ | 4µs | 8.7 MB |
| 31 | active-directory-search-for-a-user | ✓ | 4µs | 8.7 MB |
| 32 | active-object | ✓ | 39µs | 8.7 MB |
| 33 | add-a-variable-to-a-class-instance-at-runtime | ✓ | 5µs | 8.7 MB |
| 34 | additive-primes | ✓ | 6µs | 8.7 MB |
| 35 | address-of-a-variable | ✓ | 8µs | 8.7 MB |
| 36 | adfgvx-cipher | ✓ | 15µs | 8.7 MB |
| 37 | aks-test-for-primes | ✓ | 5µs | 8.7 MB |
| 38 | algebraic-data-types | ✓ | 7µs | 8.7 MB |
| 39 | align-columns | ✓ | 9µs | 8.7 MB |
| 40 | aliquot-sequence-classifications | ✓ | 20µs | 8.7 MB |
| 41 | almkvist-giullera-formula-for-pi | ✓ | 252µs | 8.7 MB |
| 42 | almost-prime | ✓ | 4µs | 8.7 MB |
| 43 | amb | ✓ | 3µs | 8.7 MB |
| 44 | amicable-pairs | ✓ | 388µs | 8.7 MB |
| 45 | anagrams-deranged-anagrams | ✓ | 7µs | 8.7 MB |
| 46 | anagrams | ✓ | 10µs | 8.7 MB |
| 47 | angle-difference-between-two-bearings-1 | ✓ | 5µs | 8.7 MB |
| 48 | angle-difference-between-two-bearings-2 | ✓ | 5µs | 8.7 MB |
| 49 | angles-geometric-normalization-and-conversion | ✓ | 9µs | 8.8 MB |
| 50 | animate-a-pendulum | ✓ | 17µs | 8.7 MB |
| 51 | animation | ✓ | 8µs | 8.7 MB |
| 52 | anonymous-recursion-1 | ✓ | 3µs | 8.7 MB |
| 53 | anonymous-recursion-2 | ✓ | 3µs | 8.7 MB |
| 54 | anonymous-recursion | ✓ | 2µs | 8.7 MB |
| 55 | anti-primes | ✓ | 31µs | 8.7 MB |
| 56 | append-a-record-to-the-end-of-a-text-file | ✓ | 2µs | 8.7 MB |
| 57 | apply-a-callback-to-an-array-1 | ✓ | 4µs | 8.7 MB |
| 58 | apply-a-callback-to-an-array-2 | ✓ | 4µs | 8.7 MB |
| 59 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 8µs | 8.7 MB |
| 60 | approximate-equality | ✓ | 5µs | 8.7 MB |
| 61 | arbitrary-precision-integers-included- | ✓ | 237µs | 105.4 KB |
| 62 | archimedean-spiral | ✓ | 261µs | 8.7 MB |
| 63 | arena-storage-pool | ✓ | 7µs | 8.7 MB |
| 64 | arithmetic-complex | ✓ | 5µs | 8.7 MB |
| 65 | arithmetic-derivative | ✓ | 14µs | 8.7 MB |
| 66 | arithmetic-evaluation | ✓ | 4µs | 8.7 MB |
| 67 | arithmetic-geometric-mean-calculate-pi | ✓ | 4µs | 8.7 MB |
| 68 | arithmetic-geometric-mean | ✓ | 3µs | 8.7 MB |
| 69 | arithmetic-integer-1 | ✓ | 2µs | 8.7 MB |
| 70 | arithmetic-integer-2 | ✓ | 2µs | 8.7 MB |
| 71 | arithmetic-numbers | ✓ |  |  |
| 72 | arithmetic-rational | ✓ | 6µs | 8.7 MB |
| 73 | array-concatenation | ✓ | 4µs | 8.7 MB |
| 74 | array-length | ✓ | 2µs | 8.7 MB |
| 75 | arrays | ✓ | 5µs | 8.7 MB |
| 76 | ascending-primes | ✓ | 7µs | 8.7 MB |
| 77 | ascii-art-diagram-converter | ✓ | 3µs | 8.7 MB |
| 78 | assertions | ✓ | 2µs | 8.7 MB |
| 79 | associative-array-creation | ✓ | 7µs | 8.7 MB |
| 80 | associative-array-iteration | ✓ | 4µs | 3.5 MB |
| 81 | associative-array-merging | ✓ | 5µs | 3.5 MB |
| 82 | atomic-updates | ✓ | 10µs | 3.5 MB |
| 83 | attractive-numbers | ✓ | 1µs | 3.5 MB |
| 84 | average-loop-length | ✓ | 170µs | 3.5 MB |
| 85 | averages-arithmetic-mean | ✓ | 9µs | 3.5 MB |
| 86 | averages-mean-time-of-day | ✓ | 24µs | 3.5 MB |
| 87 | averages-median-1 | ✓ | 4µs | 3.5 MB |
| 88 | averages-median-2 | ✓ | 4µs | 3.5 MB |
| 89 | averages-median-3 | ✓ | 8µs | 3.5 MB |
| 90 | averages-mode | ✓ | 10µs | 3.5 MB |
| 91 | averages-pythagorean-means | ✓ | 10µs | 3.5 MB |
| 92 | averages-root-mean-square | ✓ | 1µs | 3.5 MB |
| 93 | averages-simple-moving-average | ✓ | 6µs | 3.5 MB |
| 94 | avl-tree | ✓ | 10µs | 3.5 MB |
| 95 | b-zier-curves-intersections | ✓ | 56µs | 3.5 MB |
| 96 | babbage-problem | ✓ | 4µs | 3.5 MB |
| 97 | babylonian-spiral | ✓ | 19µs | 3.5 MB |
| 98 | balanced-brackets | ✓ | 8µs | 3.5 MB |
| 99 | balanced-ternary | ✓ | 5µs | 3.5 MB |
| 100 | barnsley-fern | ✓ | 31µs | 3.5 MB |
| 101 | base64-decode-data | ✓ | 9µs | 3.5 MB |
| 102 | bell-numbers | ✓ | 17µs | 3.5 MB |
| 103 | benfords-law | ✓ | 29µs | 3.5 MB |
| 104 | bernoulli-numbers | ✓ | 92µs | 3.5 MB |
| 105 | best-shuffle | ✓ | 6µs | 3.5 MB |
| 106 | bifid-cipher | ✓ | 14µs | 3.5 MB |
| 107 | bin-given-limits | ✓ | 10µs | 3.5 MB |
| 108 | binary-digits | ✓ | 2µs | 3.5 MB |
| 109 | binary-search | ✓ | 5µs | 3.5 MB |
| 110 | binary-strings |   |  |  |
| 111 | bioinformatics-base-count | ✓ | 4µs | 8.7 MB |
| 112 | bioinformatics-global-alignment | ✓ | 199µs | 8.7 MB |
| 113 | bioinformatics-sequence-mutation | ✓ | 21µs | 8.7 MB |
| 114 | biorhythms | ✓ | 11µs | 8.7 MB |
| 115 | bitcoin-address-validation | ✓ | 25µs | 8.9 MB |
| 116 | bitmap-b-zier-curves-cubic | error |  |  |
| 117 | bitmap-b-zier-curves-quadratic | error |  |  |
| 118 | bitmap-bresenhams-line-algorithm | ✓ | 7µs | 8.7 MB |
| 119 | bitmap-flood-fill | ✓ | 4µs | 8.7 MB |
| 120 | bitmap-histogram | ✓ | 5µs | 8.7 MB |
| 121 | bitmap-midpoint-circle-algorithm | ✓ | 28µs | 8.7 MB |
| 122 | bitmap-ppm-conversion-through-a-pipe | ✓ | 81µs | 8.7 MB |
| 123 | bitmap-read-a-ppm-file | error |  |  |
| 124 | bitmap-read-an-image-through-a-pipe | error |  |  |
| 125 | bitmap-write-a-ppm-file | ✓ | 6µs | 8.7 MB |
| 126 | bitmap | error |  |  |
| 127 | bitwise-io-1 | ✓ | 3µs | 8.7 MB |
| 128 | bitwise-io-2 | error |  |  |
| 129 | bitwise-operations | ✓ | 6µs | 8.7 MB |
| 130 | blum-integer | ✓ | 7µs | 8.7 MB |
| 131 | boolean-values | ✓ | 6µs | 8.7 MB |
| 132 | box-the-compass | ✓ | 7µs | 8.7 MB |
| 133 | boyer-moore-string-search | ✓ | 39µs | 8.7 MB |
| 134 | brazilian-numbers | ✓ | 2.52ms | 8.7 MB |
| 135 | break-oo-privacy | ✓ | 3µs | 8.7 MB |
| 136 | brilliant-numbers | error |  |  |
| 137 | brownian-tree | ✓ |  |  |
| 138 | bulls-and-cows-player | ✓ | 39µs | 8.7 MB |
| 139 | bulls-and-cows | error |  |  |
| 140 | burrows-wheeler-transform | ✓ | 25µs | 8.7 MB |
| 141 | caesar-cipher-1 | ✓ | 8µs | 8.7 MB |
| 142 | caesar-cipher-2 | ✓ | 11µs | 8.7 MB |
| 143 | calculating-the-value-of-e | error |  |  |
| 144 | calendar---for-real-programmers-1 | ✓ | 56µs | 8.7 MB |
| 145 | calendar---for-real-programmers-2 | ✓ | 47µs | 8.7 MB |
| 146 | calendar | ✓ | 54µs | 8.7 MB |
| 147 | calkin-wilf-sequence | error |  |  |
| 148 | call-a-foreign-language-function | ✓ | 2µs | 8.7 MB |
| 149 | call-a-function-1 | ✓ |  | 8.7 MB |
| 150 | call-a-function-10 | ✓ | 1µs | 8.7 MB |
| 151 | call-a-function-11 | ✓ | 3µs | 8.7 MB |
| 152 | call-a-function-12 | ✓ | 3µs | 8.7 MB |
| 153 | call-a-function-2 | ✓ | 3µs | 8.7 MB |
| 154 | call-a-function-3 | ✓ | 7µs | 8.7 MB |
| 155 | call-a-function-4 | ✓ | 1µs | 8.7 MB |
| 156 | call-a-function-5 | ✓ | 3µs | 8.7 MB |
| 157 | call-a-function-6 | ✓ | 9µs | 8.7 MB |
| 158 | call-a-function-7 | ✓ |  | 8.7 MB |
| 159 | call-a-function-8 | error |  |  |
| 160 | call-a-function-9 | ✓ | 3µs | 8.7 MB |
| 161 | call-an-object-method-1 | ✓ | 1µs | 8.7 MB |
| 162 | call-an-object-method-2 | ✓ | 2µs | 8.7 MB |
| 163 | call-an-object-method-3 | ✓ | 1µs | 8.7 MB |
| 164 | call-an-object-method | ✓ | 1µs | 8.7 MB |
| 165 | camel-case-and-snake-case | error |  |  |
| 166 | canny-edge-detector | error |  |  |
| 167 | canonicalize-cidr | error |  |  |
| 168 | cantor-set | error |  |  |
| 169 | carmichael-3-strong-pseudoprimes | error |  |  |
| 170 | cartesian-product-of-two-or-more-lists-1 | ✓ | 4µs | 8.7 MB |
| 171 | cartesian-product-of-two-or-more-lists-2 | ✓ | 9µs | 8.7 MB |
| 172 | cartesian-product-of-two-or-more-lists-3 | ✓ | 10µs | 8.7 MB |
| 173 | cartesian-product-of-two-or-more-lists-4 | ✓ | 10µs | 8.7 MB |
| 174 | case-sensitivity-of-identifiers | ✓ | 5µs | 8.7 MB |
| 175 | casting-out-nines | error |  |  |
| 176 | catalan-numbers-1 | ✓ | 21µs | 8.7 MB |
| 177 | catalan-numbers-2 | ✓ | 16µs | 8.7 MB |
| 178 | catalan-numbers-pascals-triangle | error |  |  |
| 179 | catamorphism | ✓ | 3µs | 8.7 MB |
| 180 | catmull-clark-subdivision-surface | error |  |  |
| 181 | chaocipher | ✓ | 4µs | 8.7 MB |
| 182 | chaos-game | error |  |  |
| 183 | character-codes-1 | ✓ | 2µs | 8.7 MB |
| 184 | character-codes-2 | ✓ | 2µs | 8.7 MB |
| 185 | character-codes-3 | ✓ | 2µs | 8.7 MB |
| 186 | character-codes-4 | ✓ | 2µs | 8.7 MB |
| 187 | character-codes-5 | ✓ | 2µs | 8.7 MB |
| 188 | chat-server | ✓ | 3µs | 8.7 MB |
| 189 | check-machin-like-formulas | ✓ | 33µs | 8.7 MB |
| 190 | check-that-file-exists | ✓ | 3µs | 8.7 MB |
| 191 | checkpoint-synchronization-1 | ✓ | 17µs | 8.7 MB |
| 192 | checkpoint-synchronization-2 | ✓ | 16µs | 8.7 MB |
| 193 | checkpoint-synchronization-3 | ✓ | 17µs | 8.7 MB |
| 194 | checkpoint-synchronization-4 | ✓ | 20µs | 8.7 MB |
| 195 | chernicks-carmichael-numbers | ✓ | 2.23ms | 8.7 MB |
| 196 | cheryls-birthday | ✓ | 5µs | 8.7 MB |
| 197 | chinese-remainder-theorem | ✓ | 22µs | 8.7 MB |
| 198 | chinese-zodiac | ✓ | 4µs | 8.7 MB |
| 199 | cholesky-decomposition-1 | error |  |  |
| 200 | cholesky-decomposition | ✓ |  |  |
| 201 | chowla-numbers | ✓ |  |  |
| 202 | church-numerals-1 | ✓ |  |  |
| 203 | church-numerals-2 | error |  |  |
| 204 | circles-of-given-radius-through-two-points | error |  |  |
| 205 | circular-primes | error |  |  |
| 206 | cistercian-numerals | error |  |  |
| 207 | comma-quibbling | ✓ |  |  |
| 208 | compiler-virtual-machine-interpreter | error |  |  |
| 209 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k | error |  |  |
| 210 | compound-data-type | ✓ |  |  |
| 211 | concurrent-computing-1 | error |  |  |
| 212 | concurrent-computing-2 | error |  |  |
| 213 | concurrent-computing-3 | error |  |  |
| 214 | conditional-structures-1 | ✓ |  |  |
| 215 | conditional-structures-10 | ✓ |  |  |
| 216 | conditional-structures-2 | ✓ |  |  |
| 217 | conditional-structures-3 | ✓ |  |  |
| 218 | conditional-structures-4 | ✓ |  |  |
| 219 | conditional-structures-5 | ✓ |  |  |
| 220 | conditional-structures-6 | error |  |  |
| 221 | conditional-structures-7 | ✓ |  | 8.7 MB |
| 222 | conditional-structures-8 | ✓ |  | 8.7 MB |
| 223 | conditional-structures-9 | ✓ |  | 8.7 MB |
| 224 | consecutive-primes-with-ascending-or-descending-differences | error |  |  |
| 225 | constrained-genericity-1 | ✓ |  | 8.7 MB |
| 226 | constrained-genericity-2 | error |  |  |
| 227 | constrained-genericity-3 | ✓ |  | 8.7 MB |
| 228 | constrained-genericity-4 | ✓ | 3µs | 8.7 MB |
| 229 | constrained-random-points-on-a-circle-1 | error |  |  |
| 230 | constrained-random-points-on-a-circle-2 | error |  |  |
| 231 | continued-fraction | ✓ | 10µs | 8.7 MB |
| 232 | convert-decimal-number-to-rational | error |  |  |
| 233 | convert-seconds-to-compound-duration | ✓ | 3µs | 8.7 MB |
| 234 | convex-hull | ✓ | 21µs | 8.7 MB |
| 235 | conways-game-of-life | ✓ | 851µs | 8.7 MB |
| 236 | copy-a-string-1 | ✓ |  | 8.7 MB |
| 237 | copy-a-string-2 | ✓ | 3µs | 8.7 MB |
| 238 | copy-stdin-to-stdout-1 | ✓ | 15µs | 8.7 MB |
| 239 | copy-stdin-to-stdout-2 | ✓ | 3µs | 8.7 MB |
| 240 | count-in-factors | error |  |  |
| 241 | count-in-octal-1 | ✓ | 23µs | 8.8 MB |
| 242 | count-in-octal-2 | ✓ | 245µs | 8.7 MB |
| 243 | count-in-octal-3 | error |  |  |
| 244 | count-in-octal-4 | ✓ | 16µs | 8.8 MB |
| 245 | count-occurrences-of-a-substring | error |  |  |
| 246 | count-the-coins-1 | ✓ | 3µs | 8.7 MB |
| 247 | count-the-coins-2 | ✓ | 33µs | 8.7 MB |
| 248 | cramers-rule | ✓ |  |  |
| 249 | crc-32-1 | error |  |  |
| 250 | crc-32-2 | error |  |  |
| 251 | create-a-file-on-magnetic-tape | ✓ | 2µs | 8.7 MB |
| 252 | create-a-file | ✓ | 2µs | 8.7 MB |
| 253 | create-a-two-dimensional-array-at-runtime-1 | ✓ | 4µs | 8.7 MB |
| 254 | create-an-html-table | ✓ | 16µs | 8.7 MB |
| 255 | create-an-object-at-a-given-address | ✓ | 20µs | 8.7 MB |
| 256 | csv-data-manipulation | ✓ | 11µs | 8.7 MB |
| 257 | csv-to-html-translation-1 | ✓ | 3µs | 8.7 MB |
| 258 | csv-to-html-translation-2 | ✓ | 5µs | 8.7 MB |
| 259 | csv-to-html-translation-3 | ✓ | 2µs | 8.7 MB |
| 260 | csv-to-html-translation-4 | ✓ | 2µs | 8.7 MB |
| 261 | csv-to-html-translation-5 | ✓ | 6µs | 8.7 MB |
| 262 | cuban-primes | ✓ | 13.374ms | 8.7 MB |
| 263 | cullen-and-woodall-numbers | ✓ | 6µs | 8.7 MB |
| 264 | cumulative-standard-deviation | ✓ | 6µs | 8.7 MB |
| 265 | currency | ✓ | 7µs | 8.7 MB |
| 266 | currying | ✓ | 6µs | 8.7 MB |
| 267 | curzon-numbers | ✓ | 414µs | 8.7 MB |
| 268 | cusip | error |  |  |
| 269 | cyclops-numbers |   |  |  |
| 270 | damm-algorithm | ✓ | 8µs | 8.7 MB |
| 271 | date-format |   |  |  |
| 272 | date-manipulation |   |  |  |
| 273 | day-of-the-week |   |  |  |
| 274 | de-bruijn-sequences |   |  |  |
| 275 | deal-cards-for-freecell |   |  |  |
| 276 | death-star |   |  |  |
| 277 | deceptive-numbers |   |  |  |
| 278 | deconvolution-1d-2 |   |  |  |
| 279 | deconvolution-1d-3 |   |  |  |
| 280 | deconvolution-1d |   |  |  |
| 281 | deepcopy-1 |   |  |  |
| 282 | define-a-primitive-data-type |   |  |  |
| 283 | delegates |   |  |  |
| 284 | demings-funnel |   |  |  |
| 285 | department-numbers |   |  |  |
| 286 | descending-primes |   |  |  |
| 287 | detect-division-by-zero |   |  |  |
| 288 | determine-if-a-string-has-all-the-same-characters |   |  |  |
| 289 | determine-if-a-string-has-all-unique-characters |   |  |  |
| 290 | determine-if-a-string-is-collapsible |   |  |  |
| 291 | determine-if-a-string-is-numeric-1 |   |  |  |
| 292 | determine-if-a-string-is-numeric-2 |   |  |  |
| 293 | determine-if-a-string-is-squeezable |   |  |  |
| 294 | determine-if-only-one-instance-is-running |   |  |  |
| 295 | determine-if-two-triangles-overlap |   |  |  |
| 296 | determine-sentence-type |   |  |  |
| 297 | dice-game-probabilities-1 | ✓ | 240µs | 8.7 MB |
| 298 | dice-game-probabilities-2 | ✓ |  |  |
| 299 | digital-root-multiplicative-digital-root | ✓ |  |  |
| 300 | dijkstras-algorithm | error |  |  |
| 301 | dinesmans-multiple-dwelling-problem |   |  |  |
| 302 | dining-philosophers-1 |   |  |  |
| 303 | dining-philosophers-2 |   |  |  |
| 304 | disarium-numbers |   |  |  |
| 305 | discordian-date |   |  |  |
| 306 | display-a-linear-combination |   |  |  |
| 307 | display-an-outline-as-a-nested-table |   |  |  |
| 308 | distance-and-bearing |   |  |  |
| 309 | distributed-programming |   |  |  |
| 310 | diversity-prediction-theorem |   |  |  |
| 311 | dns-query |   |  |  |
| 312 | documentation |   |  |  |
| 313 | doomsday-rule |   |  |  |
| 314 | dot-product |   |  |  |
| 315 | doubly-linked-list-definition-1 |   |  |  |
| 316 | doubly-linked-list-definition-2 |   |  |  |
| 317 | doubly-linked-list-element-definition |   |  |  |
| 318 | doubly-linked-list-traversal |   |  |  |
| 319 | dragon-curve |   |  |  |
| 320 | draw-a-clock |   |  |  |
| 321 | draw-a-cuboid |   |  |  |
| 322 | draw-a-pixel-1 |   |  |  |
| 323 | draw-a-rotating-cube |   |  |  |
| 324 | draw-a-sphere |   |  |  |
| 325 | duffinian-numbers |   |  |  |
| 326 | dutch-national-flag-problem |   |  |  |
| 327 | dynamic-variable-names |   |  |  |
| 328 | earliest-difference-between-prime-gaps |   |  |  |
| 329 | eban-numbers |   |  |  |
| 330 | ecdsa-example |   |  |  |
| 331 | echo-server |   |  |  |
| 332 | eertree |   |  |  |
| 333 | egyptian-division |   |  |  |
| 334 | ekg-sequence-convergence |   |  |  |
| 335 | element-wise-operations |   |  |  |
| 336 | elementary-cellular-automaton-infinite-length |   |  |  |
| 337 | elementary-cellular-automaton-random-number-generator |   |  |  |
| 338 | elementary-cellular-automaton |   |  |  |
| 339 | elliptic-curve-arithmetic |   |  |  |
| 340 | elliptic-curve-digital-signature-algorithm |   |  |  |
| 341 | emirp-primes |   |  |  |
| 342 | empty-directory |   |  |  |
| 343 | empty-program |   |  |  |
| 344 | empty-string-1 |   |  |  |
| 345 | empty-string-2 |   |  |  |
| 346 | enforced-immutability |   |  |  |
| 347 | entropy-1 |   |  |  |
| 348 | entropy-2 |   |  |  |
| 349 | entropy-narcissist |   |  |  |
| 350 | enumerations-1 |   |  |  |
| 351 | enumerations-2 |   |  |  |
| 352 | enumerations-3 |   |  |  |
| 353 | enumerations-4 |   |  |  |
| 354 | environment-variables-1 |   |  |  |
| 355 | environment-variables-2 |   |  |  |
| 356 | equal-prime-and-composite-sums |   |  |  |
| 357 | equilibrium-index |   |  |  |
| 358 | erd-s-nicolas-numbers |   |  |  |
| 359 | erd-s-selfridge-categorization-of-primes |   |  |  |
| 360 | esthetic-numbers |   |  |  |
| 361 | ethiopian-multiplication |   |  |  |
| 362 | euclid-mullin-sequence |   |  |  |
| 363 | euler-method |   |  |  |
| 364 | eulers-constant-0.5772... |   |  |  |
| 365 | eulers-identity |   |  |  |
| 366 | eulers-sum-of-powers-conjecture |   |  |  |
| 367 | evaluate-binomial-coefficients |   |  |  |
| 368 | even-or-odd |   |  |  |
| 369 | events |   |  |  |
| 370 | evolutionary-algorithm |   |  |  |
| 371 | exceptions-catch-an-exception-thrown-in-a-nested-call |   |  |  |
| 372 | exceptions |   |  |  |
| 373 | executable-library |   |  |  |
| 374 | execute-a-markov-algorithm |   |  |  |
| 375 | execute-a-system-command |   |  |  |
| 376 | execute-brain- |   |  |  |
| 377 | execute-computer-zero-1 |   |  |  |
| 378 | execute-computer-zero |   |  |  |
| 379 | execute-hq9+ |   |  |  |
| 380 | execute-snusp |   |  |  |
| 381 | exponentiation-operator-2 |   |  |  |
| 382 | exponentiation-operator |   |  |  |
| 383 | exponentiation-order |   |  |  |
| 384 | exponentiation-with-infix-operators-in-or-operating-on-the-base |   |  |  |
| 385 | extend-your-language |   |  |  |
| 386 | extensible-prime-generator |   |  |  |
| 387 | extreme-floating-point-values |   |  |  |
| 388 | faces-from-a-mesh-2 |   |  |  |
| 389 | faces-from-a-mesh |   |  |  |
| 390 | factorial-base-numbers-indexing-permutations-of-a-collection |   |  |  |
| 391 | factorial-primes |   |  |  |
| 392 | factorial |   |  |  |
| 393 | factorions |   |  |  |
| 394 | factors-of-a-mersenne-number |   |  |  |
| 395 | factors-of-an-integer |   |  |  |
| 396 | fairshare-between-two-and-more |   |  |  |
| 397 | farey-sequence |   |  |  |
| 398 | fast-fourier-transform |   |  |  |
| 399 | fasta-format | ✓ | 7µs | 8.7 MB |
| 400 | faulhabers-formula | ✓ | 28µs | 8.7 MB |
| 401 | faulhabers-triangle | ✓ | 39µs | 8.7 MB |
| 402 | feigenbaum-constant-calculation | ✓ | 280µs | 8.7 MB |
| 403 | fermat-numbers | ✓ | 33µs | 8.7 MB |
| 404 | fibonacci-n-step-number-sequences | ✓ | 7µs | 8.7 MB |
| 405 | fibonacci-sequence-1 | ✓ |  | 8.7 MB |
| 406 | fibonacci-sequence-2 | ✓ |  | 8.7 MB |
| 407 | fibonacci-sequence-3 | ✓ |  | 8.7 MB |
| 408 | fibonacci-sequence-4 | ✓ | 5µs | 8.7 MB |
| 409 | fibonacci-sequence-5 | ✓ | 8µs | 8.7 MB |
| 410 | fibonacci-word-fractal | ✓ | 5µs | 8.7 MB |
| 411 | fibonacci-word | ✓ | 8.398ms | 8.7 MB |
| 412 | file-extension-is-in-extensions-list | ✓ | 10µs | 8.7 MB |
| 413 | file-input-output-1 | ✓ | 4µs | 8.7 MB |
| 414 | file-input-output-2 | ✓ | 4µs | 8.7 MB |
| 415 | file-input-output-3 | ✓ | 6µs | 8.7 MB |
| 416 | file-modification-time | ✓ | 5µs | 8.7 MB |
| 417 | file-size-distribution | ✓ | 8µs | 8.7 MB |
| 418 | file-size | ✓ | 4µs | 8.7 MB |
| 419 | filter | ✓ | 14µs | 8.7 MB |
| 420 | find-chess960-starting-position-identifier-2 | ✓ |  |  |
| 421 | find-chess960-starting-position-identifier | ✓ |  |  |
| 422 | find-common-directory-path | ✓ | 11µs | 8.7 MB |
| 423 | find-duplicate-files | ✓ | 5µs | 8.7 MB |
| 424 | find-largest-left-truncatable-prime-in-a-given-base | ✓ | 412µs | 8.7 MB |
| 425 | find-limit-of-recursion | ✓ | 50µs | 8.7 MB |
| 426 | find-palindromic-numbers-in-both-binary-and-ternary-bases | ✓ |  |  |
| 427 | find-the-intersection-of-a-line-with-a-plane | ✓ | 6µs | 8.7 MB |
| 428 | find-the-intersection-of-two-lines | ✓ | 7µs | 8.8 MB |
| 429 | find-the-last-sunday-of-each-month | ✓ |  |  |
| 430 | find-the-missing-permutation | ✓ | 8µs | 8.7 MB |
| 431 | first-class-environments | ✓ | 35µs | 8.7 MB |
| 432 | first-class-functions-use-numbers-analogously | ✓ | 5µs | 8.7 MB |
| 433 | first-power-of-2-that-has-leading-decimal-digits-of-12 | ✓ | 27µs | 8.7 MB |
| 434 | five-weekends | ✓ | 46µs | 8.7 MB |
| 435 | fivenum-1 | ✓ | 6µs | 8.8 MB |
| 436 | fivenum-2 | ✓ | 11µs | 8.7 MB |
| 437 | fivenum-3 | ✓ | 9µs | 8.7 MB |
| 438 | fixed-length-records-1 | ✓ | 7µs | 8.7 MB |
| 439 | fixed-length-records-2 | ✓ | 11µs | 8.7 MB |
| 440 | fizzbuzz-1 | ✓ | 23µs | 8.8 MB |
| 441 | fizzbuzz-2 | ✓ | 26µs | 8.8 MB |
| 442 | fizzbuzz | ✓ | 21µs | 8.8 MB |
| 443 | flatten-a-list-1 | ✓ | 4µs | 8.7 MB |
| 444 | flatten-a-list-2 | ✓ | 2µs | 8.7 MB |
| 445 | flipping-bits-game | error |  |  |
| 446 | flow-control-structures-1 | ✓ | 3µs | 8.7 MB |
| 447 | flow-control-structures-2 | ✓ | 2µs | 8.7 MB |
| 448 | flow-control-structures-3 | ✓ | 17µs | 8.7 MB |
| 449 | flow-control-structures-4 | ✓ | 2µs | 8.7 MB |
| 450 | floyd-warshall-algorithm | error |  |  |
| 451 | floyd-warshall-algorithm2 | error |  |  |
| 452 | floyds-triangle | error |  |  |
| 453 | forest-fire | ✓ | 66µs | 8.7 MB |
| 454 | fork-2 | ✓ | 3µs | 8.7 MB |
| 455 | fork | ✓ | 3µs | 8.7 MB |
| 456 | formal-power-series | error |  |  |
| 457 | formatted-numeric-output | ✓ | 5µs | 8.7 MB |
| 458 | forward-difference | ✓ | 4µs | 8.7 MB |
| 459 | four-bit-adder-1 | ✓ | 4µs | 8.7 MB |
| 460 | four-is-magic | ✓ | 7µs | 8.7 MB |
| 461 | four-is-the-number-of-letters-in-the-... | error |  |  |
| 462 | fractal-tree | ✓ | 30µs | 8.8 MB |
| 463 | fractran | ✓ | 2.549ms | 8.7 MB |
| 464 | french-republican-calendar | ✓ | 4µs | 8.7 MB |
| 465 | ftp | ✓ | 7µs | 8.7 MB |
| 466 | function-frequency | error |  |  |
| 467 | function-prototype | ✓ |  | 8.7 MB |
| 468 | functional-coverage-tree | error |  |  |
| 469 | fusc-sequence | ✓ | 9µs | 8.7 MB |
| 470 | gamma-function | ✓ | 24µs | 8.7 MB |
| 471 | general-fizzbuzz | ✓ | 19µs | 8.8 MB |
| 472 | generic-swap | error |  |  |
| 473 | get-system-command-output | ✓ | 2µs | 8.7 MB |
| 474 | giuga-numbers | ✓ | 6µs | 8.7 MB |
| 475 | globally-replace-text-in-several-files | ✓ | 2µs | 8.7 MB |
| 476 | goldbachs-comet | ✓ | 17µs | 8.7 MB |
| 477 | golden-ratio-convergence | ✓ | 4µs | 8.7 MB |
| 478 | graph-colouring | ✓ | 2µs | 8.7 MB |
| 479 | gray-code | ✓ | 23µs | 8.7 MB |
| 480 | gui-component-interaction | ✓ | 10µs | 8.7 MB |
| 481 | gui-enabling-disabling-of-controls | ✓ | 5µs | 8.7 MB |
| 482 | gui-maximum-window-dimensions | ✓ | 4µs | 8.7 MB |
| 483 | http |   |  |  |
| 484 | image-noise | ✓ | 364µs | 8.7 MB |
| 485 | loops-increment-loop-index-within-loop-body | ✓ | 1.532ms | 8.7 MB |
| 486 | md5 | ✓ | 52µs | 8.9 MB |
| 487 | nim-game | ✓ | 9µs | 8.7 MB |
| 488 | plasma-effect | error |  |  |
| 489 | sorting-algorithms-bubble-sort | ✓ | 6µs | 8.7 MB |
| 490 | window-management | ✓ | 5µs | 8.7 MB |
| 491 | zumkeller-numbers | ✓ | 38.459ms | 8.8 MB |
