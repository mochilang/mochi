# C++ Transpiler Rosetta Output

This directory stores C++ code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.cpp` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (129/332) - Last updated 2025-07-27 00:58 +0700:
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
| 139 | brownian-tree | ✓ | -1993291818000ns | 13.56MB |
| 140 | bulls-and-cows-player | ✓ |  |  |
| 141 | bulls-and-cows | ✓ |  |  |
| 142 | burrows-wheeler-transform | ✓ |  |  |
| 143 | caesar-cipher-1 | ✓ |  |  |
| 144 | caesar-cipher-2 | ✓ |  |  |
| 145 | calculating-the-value-of-e | ✓ |  |  |
| 146 | calendar---for-real-programmers-1 | ✓ |  |  |
| 147 | calendar---for-real-programmers-2 | ✓ |  |  |
| 148 | calendar | ✓ |  |  |
| 149 | calkin-wilf-sequence |   |  |  |
| 150 | call-a-foreign-language-function | ✓ |  |  |
| 151 | call-a-function-1 | ✓ |  |  |
| 152 | call-a-function-10 | ✓ |  |  |
| 153 | call-a-function-11 |   |  |  |
| 154 | call-a-function-12 |   |  |  |
| 155 | call-a-function-2 |   |  |  |
| 156 | call-a-function-3 |   |  |  |
| 157 | call-a-function-4 |   |  |  |
| 158 | call-a-function-5 |   |  |  |
| 159 | call-a-function-6 |   |  |  |
| 160 | call-a-function-7 |   |  |  |
| 161 | call-a-function-8 |   |  |  |
| 162 | call-a-function-9 |   |  |  |
| 163 | call-an-object-method-1 |   |  |  |
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
| 330 | echo-server |   |  |  |
| 331 | ekg-sequence-convergence |   |  |  |
| 332 | md5 |   |  |  |
