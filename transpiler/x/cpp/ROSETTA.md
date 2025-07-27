# C++ Transpiler Rosetta Output

This directory stores C++ code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.cpp` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (140/465) - Last updated 2025-07-27 16:50 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 1 | 100-doors-2 | ✓ | 222.0µs | 13.12MB |
| 2 | 100-doors-3 | ✓ | 206.0µs | 12.62MB |
| 3 | 100-doors | ✓ | 485.0µs | 12.42MB |
| 4 | 100-prisoners | ✓ | 915.0ms | 12.78MB |
| 5 | 15-puzzle-game | ✓ | 258.0µs | 14.07MB |
| 6 | 15-puzzle-solver | ✓ | 141.0µs | 12.91MB |
| 7 | 2048 | ✓ | 689.0µs | 13.73MB |
| 8 | 21-game | ✓ | 356.0µs | 13.36MB |
| 9 | 24-game-solve | ✓ | 4.0ms | 13.75MB |
| 10 | 24-game | ✓ |  |  |
| 11 | 4-rings-or-4-squares-puzzle |   |  |  |
| 12 | 9-billion-names-of-god-the-integer | ✓ | 126.70s | 219.41MB |
| 13 | 99-bottles-of-beer-2 | ✓ | 35.0ms | 14.44MB |
| 14 | 99-bottles-of-beer | ✓ | 390.0µs | 13.00MB |
| 15 | DNS-query | ✓ | 104.0µs | 12.87MB |
| 16 | Duffinian-numbers |   |  |  |
| 17 | a+b | ✓ | 46.0µs | 12.06MB |
| 18 | abbreviations-automatic | ✓ | 11.0ms | 14.06MB |
| 19 | abbreviations-easy | ✓ | 1.0ms | 13.74MB |
| 20 | abbreviations-simple | ✓ | 3.0ms | 14.25MB |
| 21 | abc-problem | ✓ |  |  |
| 22 | abelian-sandpile-model-identity | ✓ |  |  |
| 23 | abelian-sandpile-model | ✓ | 398.0µs | 13.57MB |
| 24 | abstract-type | ✓ | 113.0µs | 13.05MB |
| 25 | abundant-deficient-and-perfect-number-classifications | ✓ | 222.0ms | 12.93MB |
| 26 | abundant-odd-numbers | ✓ | 1.10s | 12.94MB |
| 27 | accumulator-factory | ✓ | 71.0µs | 12.79MB |
| 28 | achilles-numbers | ✓ | 21.0ms | 14.59MB |
| 29 | ackermann-function-2 | ✓ | 52.0µs | 13.44MB |
| 30 | ackermann-function-3 | ✓ |  |  |
| 31 | ackermann-function | ✓ | 217.0µs | 12.62MB |
| 32 | active-directory-connect | ✓ | 44.0µs | 12.87MB |
| 33 | active-directory-search-for-a-user | ✓ | 66.0µs | 12.94MB |
| 34 | active-object | ✓ | 85.0µs | 13.00MB |
| 35 | add-a-variable-to-a-class-instance-at-runtime | ✓ |  |  |
| 36 | additive-primes | ✓ | 162.0µs | 13.29MB |
| 37 | address-of-a-variable | ✓ | 83.0µs | 12.34MB |
| 38 | adfgvx-cipher |   |  |  |
| 39 | aks-test-for-primes | ✓ | 114.0µs | 13.14MB |
| 40 | algebraic-data-types |   |  |  |
| 41 | align-columns | ✓ | 473.0µs | 13.67MB |
| 42 | aliquot-sequence-classifications | ✓ | 442.0µs | 13.93MB |
| 43 | almkvist-giullera-formula-for-pi |   |  |  |
| 44 | almost-prime | ✓ | 144.0µs | 12.99MB |
| 45 | amb | ✓ | 70.0µs | 13.10MB |
| 46 | amicable-pairs | ✓ | 326.0ms | 13.35MB |
| 47 | anagrams-deranged-anagrams | ✓ | 228.0µs | 13.68MB |
| 48 | anagrams | ✓ | 640.0µs | 13.48MB |
| 49 | angle-difference-between-two-bearings-1 | ✓ | 96.0µs | 12.93MB |
| 50 | angle-difference-between-two-bearings-2 | ✓ | 80.0µs | 12.96MB |
| 51 | angles-geometric-normalization-and-conversion | ✓ | 380.0µs | 13.27MB |
| 52 | animate-a-pendulum | ✓ | 320.0µs | 13.16MB |
| 53 | animation | ✓ | 491.0µs | 12.45MB |
| 54 | anonymous-recursion-1 | ✓ | 73.0µs | 12.67MB |
| 55 | anonymous-recursion-2 | ✓ | 68.0µs | 13.05MB |
| 56 | anonymous-recursion | ✓ | 72.0µs | 12.93MB |
| 57 | anti-primes | ✓ | 28.0ms | 12.88MB |
| 58 | append-a-record-to-the-end-of-a-text-file | ✓ | 36.0µs | 12.62MB |
| 59 | apply-a-callback-to-an-array-1 | ✓ | 57.0µs | 12.69MB |
| 60 | apply-a-callback-to-an-array-2 | ✓ | 74.0µs | 13.40MB |
| 61 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 3.0ms | 13.32MB |
| 62 | approximate-equality | ✓ | 186.0µs | 13.23MB |
| 63 | arbitrary-precision-integers-included- | ✓ | 1.0ms | 13.24MB |
| 64 | archimedean-spiral | ✓ | 9.0ms | 13.12MB |
| 65 | arena-storage-pool | ✓ |  |  |
| 66 | arithmetic-complex | ✓ | 424.0µs | 12.80MB |
| 67 | arithmetic-derivative | ✓ | 2.0ms | 13.12MB |
| 68 | arithmetic-evaluation | ✓ | 346.0µs | 13.70MB |
| 69 | arithmetic-geometric-mean-calculate-pi | ✓ | 463.0µs | 12.61MB |
| 70 | arithmetic-geometric-mean | ✓ | 440.0µs | 13.05MB |
| 71 | arithmetic-integer-1 | ✓ | 256.0µs | 12.80MB |
| 72 | arithmetic-integer-2 | ✓ | 197.0µs | 13.10MB |
| 73 | arithmetic-numbers |   |  |  |
| 74 | arithmetic-rational | ✓ | 1.0ms | 12.86MB |
| 75 | array-concatenation | ✓ | 348.0µs | 13.04MB |
| 76 | array-length | ✓ | 255.0µs | 12.74MB |
| 77 | arrays | ✓ | 394.0µs | 13.24MB |
| 78 | ascending-primes | ✓ | 917.0µs | 13.05MB |
| 79 | ascii-art-diagram-converter | ✓ | 304.0µs | 12.62MB |
| 80 | assertions | ✓ | 234.0µs | 12.75MB |
| 81 | associative-array-creation |   |  |  |
| 82 | associative-array-iteration | ✓ | 336.0µs | 13.03MB |
| 83 | associative-array-merging |   |  |  |
| 84 | atomic-updates |   |  |  |
| 85 | attractive-numbers | ✓ | 420.0µs | 12.94MB |
| 86 | average-loop-length |   |  |  |
| 87 | averages-arithmetic-mean |   |  |  |
| 88 | averages-mean-time-of-day | ✓ | 284.0µs | 13.93MB |
| 89 | averages-median-1 | ✓ | 413.0µs | 12.78MB |
| 90 | averages-median-2 | ✓ | 380.0µs | 12.90MB |
| 91 | averages-median-3 | ✓ | 398.0µs | 13.63MB |
| 92 | averages-mode | ✓ | 246.0µs | 13.37MB |
| 93 | averages-pythagorean-means | ✓ | 421.0µs | 13.16MB |
| 94 | averages-root-mean-square | ✓ | 326.0µs | 12.67MB |
| 95 | averages-simple-moving-average | ✓ | 517.0µs | 12.86MB |
| 96 | avl-tree |   |  |  |
| 97 | b-zier-curves-intersections | ✓ | 20.0ms | 14.11MB |
| 98 | babbage-problem | ✓ | 374.0µs | 12.66MB |
| 99 | babylonian-spiral |   |  |  |
| 100 | balanced-brackets |   |  |  |
| 101 | balanced-ternary | ✓ | 344.0µs | 14.06MB |
| 102 | barnsley-fern | ✓ | 99.0ms | 13.25MB |
| 103 | base64-decode-data | ✓ | 31.0µs | 14.43MB |
| 104 | bell-numbers | ✓ | 4.0ms | 13.08MB |
| 105 | benfords-law | ✓ | 1.0ms | 13.87MB |
| 106 | bernoulli-numbers |   |  |  |
| 107 | best-shuffle |   |  |  |
| 108 | bifid-cipher |   |  |  |
| 109 | bin-given-limits | ✓ | 531.0µs | 14.12MB |
| 110 | binary-digits | ✓ | 178.0µs | 12.68MB |
| 111 | binary-search | ✓ | 63.0µs | 12.98MB |
| 112 | binary-strings |   |  |  |
| 113 | bioinformatics-base-count | ✓ | 433.0µs | 13.55MB |
| 114 | bioinformatics-global-alignment | ✓ | 20.78s | 14.31MB |
| 115 | bioinformatics-sequence-mutation | ✓ | 58.0ms | 14.24MB |
| 116 | biorhythms | ✓ | 316.0µs | 14.71MB |
| 117 | bitcoin-address-validation | ✓ | 1.0ms | 13.25MB |
| 118 | bitmap-b-zier-curves-cubic | ✓ | 221.0ms | 14.31MB |
| 119 | bitmap-b-zier-curves-quadratic | ✓ | 187.0ms | 14.15MB |
| 120 | bitmap-bresenhams-line-algorithm | ✓ | 281.0µs | 13.12MB |
| 121 | bitmap-flood-fill | ✓ | 192.0µs | 12.69MB |
| 122 | bitmap-histogram | ✓ | 267.0µs | 13.55MB |
| 123 | bitmap-midpoint-circle-algorithm | ✓ | 374.0µs | 12.62MB |
| 124 | bitmap-ppm-conversion-through-a-pipe |   |  |  |
| 125 | bitmap-read-a-ppm-file | ✓ | 163.0µs | 14.37MB |
| 126 | bitmap-read-an-image-through-a-pipe | ✓ | 47.0µs | 12.98MB |
| 127 | bitmap-write-a-ppm-file | ✓ | 119.0µs | 13.36MB |
| 128 | bitmap |   |  |  |
| 129 | bitwise-io-1 |   | 246.0µs | 13.08MB |
| 130 | bitwise-io-2 | ✓ | 574.0µs | 14.68MB |
| 131 | bitwise-operations | ✓ | 233.0µs | 13.95MB |
| 132 | blum-integer | ✓ | 200.0µs | 14.06MB |
| 133 | boolean-values | ✓ | 48.0µs | 12.94MB |
| 134 | box-the-compass | ✓ | 488.0µs | 13.20MB |
| 135 | boyer-moore-string-search | ✓ | 280.0µs | 13.25MB |
| 136 | brazilian-numbers | ✓ | 8.61s | 13.38MB |
| 137 | break-oo-privacy | ✓ | 61.0µs | 13.20MB |
| 138 | brilliant-numbers |   |  |  |
| 139 | brownian-tree | ✓ | 174.41s | 13.12MB |
| 140 | bulls-and-cows-player | ✓ | 843.0ms | 13.92MB |
| 141 | bulls-and-cows | ✓ | 159.0µs | 13.12MB |
| 142 | burrows-wheeler-transform | ✓ | 15.0ms | 13.93MB |
| 143 | caesar-cipher-1 | ✓ | 353.0µs | 12.75MB |
| 144 | caesar-cipher-2 | ✓ | 377.0µs | 13.46MB |
| 145 | calculating-the-value-of-e | ✓ | 356.0µs | 13.03MB |
| 146 | calendar---for-real-programmers-1 | ✓ | 829.0µs | 13.11MB |
| 147 | calendar---for-real-programmers-2 | ✓ | 655.0µs | 13.56MB |
| 148 | calendar | ✓ | 704.0µs | 13.25MB |
| 149 | calkin-wilf-sequence |   | 415.0µs | 13.49MB |
| 150 | call-a-foreign-language-function | ✓ | 152.0µs | 12.61MB |
| 151 | call-a-function-1 | ✓ | 10.0µs | 12.54MB |
| 152 | call-a-function-10 | ✓ | 36.0µs | 12.54MB |
| 153 | call-a-function-11 | ✓ | 311.0µs | 12.50MB |
| 154 | call-a-function-12 | ✓ | 250.0µs | 12.89MB |
| 155 | call-a-function-2 | ✓ | 41.0µs | 12.65MB |
| 156 | call-a-function-3 | ✓ | 50.0µs | 12.73MB |
| 157 | call-a-function-4 | ✓ | 48.0µs | 12.68MB |
| 158 | call-a-function-5 | ✓ | 252.0µs | 12.96MB |
| 159 | call-a-function-6 | ✓ | 253.0µs | 12.57MB |
| 160 | call-a-function-7 | ✓ | 22.0µs | 12.79MB |
| 161 | call-a-function-8 | ✓ | 57.0µs | 12.88MB |
| 162 | call-a-function-9 | ✓ | 56.0µs | 12.79MB |
| 163 | call-an-object-method-1 | ✓ | 15.0µs | 12.30MB |
| 164 | call-an-object-method-2 |   |  |  |
| 165 | call-an-object-method-3 |   |  |  |
| 166 | call-an-object-method |   |  |  |
| 167 | camel-case-and-snake-case |   |  |  |
| 168 | canny-edge-detector |   |  |  |
| 169 | canonicalize-cidr |   |  |  |
| 170 | cantor-set |   |  |  |
| 171 | carmichael-3-strong-pseudoprimes |   |  |  |
| 172 | cartesian-product-of-two-or-more-lists-1 |   |  |  |
| 173 | cartesian-product-of-two-or-more-lists-2 |   |  |  |
| 174 | cartesian-product-of-two-or-more-lists-3 |   |  |  |
| 175 | cartesian-product-of-two-or-more-lists-4 |   |  |  |
| 176 | case-sensitivity-of-identifiers |   |  |  |
| 177 | casting-out-nines |   |  |  |
| 178 | catalan-numbers-1 |   |  |  |
| 179 | catalan-numbers-2 |   |  |  |
| 180 | catalan-numbers-pascals-triangle |   |  |  |
| 181 | catamorphism |   |  |  |
| 182 | catmull-clark-subdivision-surface |   |  |  |
| 183 | chaocipher |   |  |  |
| 184 | chaos-game |   |  |  |
| 185 | character-codes-1 |   |  |  |
| 186 | character-codes-2 |   |  |  |
| 187 | character-codes-3 |   |  |  |
| 188 | character-codes-4 |   |  |  |
| 189 | character-codes-5 |   |  |  |
| 190 | chat-server |   |  |  |
| 191 | check-machin-like-formulas |   |  |  |
| 192 | check-that-file-exists |   |  |  |
| 193 | checkpoint-synchronization-1 |   |  |  |
| 194 | checkpoint-synchronization-2 |   |  |  |
| 195 | checkpoint-synchronization-3 |   |  |  |
| 196 | checkpoint-synchronization-4 |   |  |  |
| 197 | chernicks-carmichael-numbers |   |  |  |
| 198 | cheryls-birthday |   |  |  |
| 199 | chinese-remainder-theorem |   |  |  |
| 200 | chinese-zodiac |   |  |  |
| 201 | cholesky-decomposition-1 |   |  |  |
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
| 399 | fasta-format |   |  |  |
| 400 | faulhabers-formula |   |  |  |
| 401 | faulhabers-triangle |   |  |  |
| 402 | feigenbaum-constant-calculation |   |  |  |
| 403 | fermat-numbers |   |  |  |
| 404 | fibonacci-n-step-number-sequences |   |  |  |
| 405 | fibonacci-sequence-1 |   |  |  |
| 406 | fibonacci-sequence-2 |   |  |  |
| 407 | fibonacci-sequence-3 |   |  |  |
| 408 | fibonacci-sequence-4 |   |  |  |
| 409 | fibonacci-word-fractal |   |  |  |
| 410 | fibonacci-word |   |  |  |
| 411 | file-extension-is-in-extensions-list |   |  |  |
| 412 | file-input-output-1 |   |  |  |
| 413 | file-input-output-2 |   |  |  |
| 414 | file-modification-time |   |  |  |
| 415 | file-size-distribution |   |  |  |
| 416 | file-size |   |  |  |
| 417 | filter |   |  |  |
| 418 | find-chess960-starting-position-identifier |   |  |  |
| 419 | find-common-directory-path |   |  |  |
| 420 | find-duplicate-files |   |  |  |
| 421 | find-if-a-point-is-within-a-triangle |   |  |  |
| 422 | find-largest-left-truncatable-prime-in-a-given-base |   |  |  |
| 423 | find-limit-of-recursion |   |  |  |
| 424 | find-palindromic-numbers-in-both-binary-and-ternary-bases |   |  |  |
| 425 | find-the-intersection-of-a-line-with-a-plane |   |  |  |
| 426 | find-the-intersection-of-two-lines |   |  |  |
| 427 | find-the-last-sunday-of-each-month |   |  |  |
| 428 | find-the-missing-permutation |   |  |  |
| 429 | fivenum-1 |   |  |  |
| 430 | fivenum-2 |   |  |  |
| 431 | fixed-length-records-1 |   |  |  |
| 432 | fixed-length-records-2 |   |  |  |
| 433 | fizzbuzz-1 |   |  |  |
| 434 | fizzbuzz-2 |   |  |  |
| 435 | flatten-a-list-1 |   |  |  |
| 436 | flatten-a-list-2 |   |  |  |
| 437 | flipping-bits-game |   |  |  |
| 438 | flow-control-structures-1 |   |  |  |
| 439 | flow-control-structures-2 |   |  |  |
| 440 | flow-control-structures-3 |   |  |  |
| 441 | flow-control-structures-4 |   |  |  |
| 442 | floyd-warshall-algorithm |   |  |  |
| 443 | floyds-triangle |   |  |  |
| 444 | forest-fire |   |  |  |
| 445 | fork |   |  |  |
| 446 | ftp |   |  |  |
| 447 | gamma-function |   |  |  |
| 448 | general-fizzbuzz |   |  |  |
| 449 | generic-swap |   |  |  |
| 450 | get-system-command-output |   |  |  |
| 451 | giuga-numbers |   |  |  |
| 452 | globally-replace-text-in-several-files |   |  |  |
| 453 | goldbachs-comet |   |  |  |
| 454 | golden-ratio-convergence |   |  |  |
| 455 | graph-colouring |   |  |  |
| 456 | gray-code |   |  |  |
| 457 | http |   |  |  |
| 458 | image-noise |   |  |  |
| 459 | loops-increment-loop-index-within-loop-body |   |  |  |
| 460 | md5 |   |  |  |
| 461 | nim-game |   |  |  |
| 462 | plasma-effect |   |  |  |
| 463 | sorting-algorithms-bubble-sort |   |  |  |
| 464 | window-management |   |  |  |
| 465 | zumkeller-numbers |   |  |  |
