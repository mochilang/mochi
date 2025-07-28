# C Transpiler Rosetta Output

This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (84/493) - Last updated 2025-07-28 10:49 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 1 | 100-doors-2 | ✓ | 65us | 17.2 KB |
| 2 | 100-doors-3 | ✓ |  |  |
| 3 | 100-doors | ✓ |  |  |
| 4 | 100-prisoners | ✓ |  |  |
| 5 | 15-puzzle-game | ✓ |  |  |
| 6 | 15-puzzle-solver | ✓ |  |  |
| 7 | 2048 |  |  |  |
| 8 | 21-game | ✓ | 25us | 8.8 KB |
| 9 | 24-game-solve |  |  |  |
| 10 | 24-game | ✓ | 36us | 9.1 KB |
| 11 | 4-rings-or-4-squares-puzzle | ✓ | 689us | 195.6 KB |
| 12 | 9-billion-names-of-god-the-integer |  |  |  |
| 13 | 99-bottles-of-beer-2 | ✓ | 2.69ms | 1.9 MB |
| 14 | 99-bottles-of-beer | ✓ | 156us | 35.3 KB |
| 15 | DNS-query |  |  |  |
| 16 | Duffinian-numbers |  |  |  |
| 17 | Find-if-a-point-is-within-a-triangle |  |  |  |
| 18 | a+b | ✓ | 30us | 8.7 KB |
| 19 | abbreviations-automatic | ✓ | 846us | 536.0 KB |
| 20 | abbreviations-easy | ✓ | 246us | 108.3 KB |
| 21 | abbreviations-simple | ✓ | 402us | 140.8 KB |
| 22 | abc-problem | ✓ | 238us | 69.5 KB |
| 23 | abelian-sandpile-model-identity | ✓ | 387us | 24.6 KB |
| 24 | abelian-sandpile-model | ✓ | 451us | 15.2 KB |
| 25 | abstract-type |  |  |  |
| 26 | abundant-deficient-and-perfect-number-classifications | ✓ | 317.38ms | 5.0 KB |
| 27 | abundant-odd-numbers |  |  |  |
| 28 | accumulator-factory |  |  |  |
| 29 | achilles-numbers |  |  |  |
| 30 | ackermann-function-2 | ✓ | 35us | 4.9 KB |
| 31 | ackermann-function-3 | ✓ | 41us | 6.7 KB |
| 32 | ackermann-function | ✓ | 112us | 4.9 KB |
| 33 | active-directory-connect | ✓ | 24us | 4.7 KB |
| 34 | active-directory-search-for-a-user | ✓ | 25us | 5.0 KB |
| 35 | active-object | ✓ | 124us | 4.7 KB |
| 36 | add-a-variable-to-a-class-instance-at-runtime |  |  |  |
| 37 | additive-primes | ✓ | 61us | 11.8 KB |
| 38 | address-of-a-variable | ✓ | 36us | 4.7 KB |
| 39 | adfgvx-cipher |  |  |  |
| 40 | aks-test-for-primes | ✓ | 65us | 12.7 KB |
| 41 | algebraic-data-types |  |  |  |
| 42 | align-columns |  |  |  |
| 43 | aliquot-sequence-classifications |  |  |  |
| 44 | almkvist-giullera-formula-for-pi |  |  |  |
| 45 | almost-prime | ✓ | 106us | 7.1 KB |
| 46 | amb | ✓ | 39us | 6.0 KB |
| 47 | amicable-pairs | ✓ | 224.39ms | 84.4 KB |
| 48 | anagrams-deranged-anagrams |  |  |  |
| 49 | anagrams |  |  |  |
| 50 | angle-difference-between-two-bearings-1 | ✓ | 21us | 4.7 KB |
| 51 | angle-difference-between-two-bearings-2 | ✓ | 21us | 4.7 KB |
| 52 | angles-geometric-normalization-and-conversion | ✓ | 128us | 25.5 KB |
| 53 | animate-a-pendulum | ✓ | 22us | 5.0 KB |
| 54 | animation | ✓ | 150us | 57.5 KB |
| 55 | anonymous-recursion-1 | ✓ | 29us | 5.9 KB |
| 56 | anonymous-recursion-2 | ✓ | 34us | 6.3 KB |
| 57 | anonymous-recursion | ✓ | 28us | 6.5 KB |
| 58 | anti-primes | ✓ | 28.61ms | 7.3 KB |
| 59 | append-a-record-to-the-end-of-a-text-file | ✓ | 16us | 4.7 KB |
| 60 | apply-a-callback-to-an-array-1 | ✓ | 357us | 4.8 KB |
| 61 | apply-a-callback-to-an-array-2 |  |  |  |
| 62 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 87us | 1.3 KB |
| 63 | approximate-equality | ✓ | 42us | 6.3 KB |
| 64 | arbitrary-precision-integers-included- | ✓ | 19us | 4.8 KB |
| 65 | archimedean-spiral | ✓ | 819us | 13.8 KB |
| 66 | arena-storage-pool | ✓ | 272us | 4.9 KB |
| 67 | arithmetic-complex | ✓ | 412us | 6.1 KB |
| 68 | arithmetic-derivative | ✓ | 946us | 79.3 KB |
| 69 | arithmetic-evaluation | ✓ | 710us | 6.9 KB |
| 70 | arithmetic-geometric-mean-calculate-pi | ✓ | 275us | 4.7 KB |
| 71 | arithmetic-geometric-mean | ✓ | 284us | 4.7 KB |
| 72 | arithmetic-integer-1 | ✓ | 220us | 5.8 KB |
| 73 | arithmetic-integer-2 | ✓ | 392us | 5.8 KB |
| 74 | arithmetic-numbers |  |  |  |
| 75 | arithmetic-rational | ✓ | 899us | 7.3 KB |
| 76 | array-concatenation |  |  |  |
| 77 | array-length | ✓ | 152us | 4.9 KB |
| 78 | arrays | ✓ | 358us | 12.0 KB |
| 79 | ascending-primes | ✓ | 99us | 34.5 KB |
| 80 | ascii-art-diagram-converter | ✓ | 18us | 4.7 KB |
| 81 | assertions | ✓ | 13us | 4.7 KB |
| 82 | associative-array-creation |  |  |  |
| 83 | associative-array-iteration | ✓ | 348us | 5.3 KB |
| 84 | associative-array-merging | ✓ | 78us | 4.8 KB |
| 85 | atomic-updates | ✓ | 45us | 8.4 KB |
| 86 | attractive-numbers | ✓ | 61us | 15.7 KB |
| 87 | average-loop-length |  |  |  |
| 88 | averages-arithmetic-mean |  |  |  |
| 89 | averages-mean-time-of-day |  |  |  |
| 90 | averages-median-1 |  |  |  |
| 91 | averages-median-2 | ✓ | 323us | 4.7 KB |
| 92 | averages-median-3 | ✓ | 848us | 4.9 KB |
| 93 | averages-mode | ✓ | 368us | 4.9 KB |
| 94 | averages-pythagorean-means | ✓ | 29us | 5.0 KB |
| 95 | averages-root-mean-square | ✓ | 32us | 4.7 KB |
| 96 | averages-simple-moving-average | ✓ | 70us | 10.3 KB |
| 97 | avl-tree |  |  |  |
| 98 | b-zier-curves-intersections |  |  |  |
| 99 | babbage-problem | ✓ | 79us | 4.9 KB |
| 100 | babylonian-spiral |  |  |  |
| 101 | balanced-brackets | ✓ | 199us | 10.2 KB |
| 102 | balanced-ternary | ✓ | 359us | 8.5 KB |
| 103 | barnsley-fern | ✓ | 3.69ms | 481.8 KB |
| 104 | base64-decode-data | ✓ | 501us | 80.9 KB |
| 105 | bell-numbers | ✓ | 97us | 24.1 KB |
| 106 | benfords-law | ✓ | 791us | 18.0 KB |
| 107 | bernoulli-numbers | ✓ | 38.37ms | 12.8 MB |
| 108 | best-shuffle |  |  |  |
| 109 | bifid-cipher |  |  |  |
| 110 | bin-given-limits | ✓ | 53us | 11.8 KB |
| 111 | binary-digits | ✓ | 46us | 7.7 KB |
| 112 | binary-search | ✓ | 47us | 4.9 KB |
| 113 | binary-strings | ✓ | 33us | 7.6 KB |
| 114 | bioinformatics-base-count | ✓ | 151us | 23.8 KB |
| 115 | bioinformatics-global-alignment |  |  |  |
| 116 | bioinformatics-sequence-mutation |  |  |  |
| 117 | biorhythms | ✓ | 54us | 13.2 KB |
| 118 | bitcoin-address-validation |  |  |  |
| 119 | bitmap-b-zier-curves-cubic |  |  |  |
| 120 | bitmap-b-zier-curves-quadratic |  |  |  |
| 121 | bitmap-bresenhams-line-algorithm |  |  |  |
| 122 | bitmap-flood-fill |  |  |  |
| 123 | bitmap-histogram |  |  |  |
| 124 | bitmap-midpoint-circle-algorithm |  |  |  |
| 125 | bitmap-ppm-conversion-through-a-pipe |  |  |  |
| 126 | bitmap-read-a-ppm-file |  |  |  |
| 127 | bitmap-read-an-image-through-a-pipe |  |  |  |
| 128 | bitmap-write-a-ppm-file |  |  |  |
| 129 | bitmap |  |  |  |
| 130 | bitwise-io-1 |  |  |  |
| 131 | bitwise-io-2 |  |  |  |
| 132 | bitwise-operations |  |  |  |
| 133 | blum-integer |  |  |  |
| 134 | boolean-values |  |  |  |
| 135 | box-the-compass |  |  |  |
| 136 | boyer-moore-string-search |  |  |  |
| 137 | brazilian-numbers |  |  |  |
| 138 | break-oo-privacy |  |  |  |
| 139 | brilliant-numbers |  |  |  |
| 140 | brownian-tree |  |  |  |
| 141 | bulls-and-cows-player |  |  |  |
| 142 | bulls-and-cows |  |  |  |
| 143 | burrows-wheeler-transform |  |  |  |
| 144 | caesar-cipher-1 |  |  |  |
| 145 | caesar-cipher-2 |  |  |  |
| 146 | calculating-the-value-of-e |  |  |  |
| 147 | calendar---for-real-programmers-1 |  |  |  |
| 148 | calendar---for-real-programmers-2 |  |  |  |
| 149 | calendar |  |  |  |
| 150 | calkin-wilf-sequence |  |  |  |
| 151 | call-a-foreign-language-function |  |  |  |
| 152 | call-a-function-1 |  |  |  |
| 153 | call-a-function-10 |  |  |  |
| 154 | call-a-function-11 |  |  |  |
| 155 | call-a-function-12 |  |  |  |
| 156 | call-a-function-2 |  |  |  |
| 157 | call-a-function-3 |  |  |  |
| 158 | call-a-function-4 |  |  |  |
| 159 | call-a-function-5 |  |  |  |
| 160 | call-a-function-6 |  |  |  |
| 161 | call-a-function-7 |  |  |  |
| 162 | call-a-function-8 |  |  |  |
| 163 | call-a-function-9 |  |  |  |
| 164 | call-an-object-method-1 |  |  |  |
| 165 | call-an-object-method-2 |  |  |  |
| 166 | call-an-object-method-3 |  |  |  |
| 167 | call-an-object-method |  |  |  |
| 168 | camel-case-and-snake-case |  |  |  |
| 169 | canny-edge-detector |  |  |  |
| 170 | canonicalize-cidr |  |  |  |
| 171 | cantor-set |  |  |  |
| 172 | carmichael-3-strong-pseudoprimes |  |  |  |
| 173 | cartesian-product-of-two-or-more-lists-1 |  |  |  |
| 174 | cartesian-product-of-two-or-more-lists-2 |  |  |  |
| 175 | cartesian-product-of-two-or-more-lists-3 |  |  |  |
| 176 | cartesian-product-of-two-or-more-lists-4 |  |  |  |
| 177 | case-sensitivity-of-identifiers |  |  |  |
| 178 | casting-out-nines |  |  |  |
| 179 | catalan-numbers-1 |  |  |  |
| 180 | catalan-numbers-2 |  |  |  |
| 181 | catalan-numbers-pascals-triangle |  |  |  |
| 182 | catamorphism |  |  |  |
| 183 | catmull-clark-subdivision-surface |  |  |  |
| 184 | chaocipher |  |  |  |
| 185 | chaos-game |  |  |  |
| 186 | character-codes-1 |  |  |  |
| 187 | character-codes-2 |  |  |  |
| 188 | character-codes-3 |  |  |  |
| 189 | character-codes-4 |  |  |  |
| 190 | character-codes-5 |  |  |  |
| 191 | chat-server |  |  |  |
| 192 | check-machin-like-formulas |  |  |  |
| 193 | check-that-file-exists |  |  |  |
| 194 | checkpoint-synchronization-1 |  |  |  |
| 195 | checkpoint-synchronization-2 |  |  |  |
| 196 | checkpoint-synchronization-3 |  |  |  |
| 197 | checkpoint-synchronization-4 |  |  |  |
| 198 | chernicks-carmichael-numbers |  |  |  |
| 199 | cheryls-birthday |  |  |  |
| 200 | chinese-remainder-theorem |  |  |  |
| 201 | chinese-zodiac |  |  |  |
| 202 | cholesky-decomposition-1 |  |  |  |
| 203 | cholesky-decomposition |  |  |  |
| 204 | chowla-numbers |  |  |  |
| 205 | church-numerals-1 |  |  |  |
| 206 | church-numerals-2 |  |  |  |
| 207 | circles-of-given-radius-through-two-points |  |  |  |
| 208 | circular-primes |  |  |  |
| 209 | cistercian-numerals |  |  |  |
| 210 | comma-quibbling |  |  |  |
| 211 | compiler-virtual-machine-interpreter |  |  |  |
| 212 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k |  |  |  |
| 213 | compound-data-type |  |  |  |
| 214 | concurrent-computing-1 |  |  |  |
| 215 | concurrent-computing-2 |  |  |  |
| 216 | concurrent-computing-3 |  |  |  |
| 217 | conditional-structures-1 |  |  |  |
| 218 | conditional-structures-10 |  |  |  |
| 219 | conditional-structures-2 |  |  |  |
| 220 | conditional-structures-3 |  |  |  |
| 221 | conditional-structures-4 |  |  |  |
| 222 | conditional-structures-5 |  |  |  |
| 223 | conditional-structures-6 |  |  |  |
| 224 | conditional-structures-7 |  |  |  |
| 225 | conditional-structures-8 |  |  |  |
| 226 | conditional-structures-9 |  |  |  |
| 227 | consecutive-primes-with-ascending-or-descending-differences |  |  |  |
| 228 | constrained-genericity-1 |  |  |  |
| 229 | constrained-genericity-2 |  |  |  |
| 230 | constrained-genericity-3 |  |  |  |
| 231 | constrained-genericity-4 |  |  |  |
| 232 | constrained-random-points-on-a-circle-1 |  |  |  |
| 233 | constrained-random-points-on-a-circle-2 |  |  |  |
| 234 | continued-fraction |  |  |  |
| 235 | convert-decimal-number-to-rational |  |  |  |
| 236 | convert-seconds-to-compound-duration |  |  |  |
| 237 | convex-hull |  |  |  |
| 238 | conways-game-of-life |  |  |  |
| 239 | copy-a-string-1 |  |  |  |
| 240 | copy-a-string-2 |  |  |  |
| 241 | copy-stdin-to-stdout-1 |  |  |  |
| 242 | copy-stdin-to-stdout-2 |  |  |  |
| 243 | count-in-factors |  |  |  |
| 244 | count-in-octal-1 |  |  |  |
| 245 | count-in-octal-2 |  |  |  |
| 246 | count-in-octal-3 |  |  |  |
| 247 | count-in-octal-4 |  |  |  |
| 248 | count-occurrences-of-a-substring |  |  |  |
| 249 | count-the-coins-1 |  |  |  |
| 250 | count-the-coins-2 |  |  |  |
| 251 | cramers-rule |  |  |  |
| 252 | crc-32-1 |  |  |  |
| 253 | crc-32-2 |  |  |  |
| 254 | create-a-file-on-magnetic-tape |  |  |  |
| 255 | create-a-file |  |  |  |
| 256 | create-a-two-dimensional-array-at-runtime-1 |  |  |  |
| 257 | create-an-html-table |  |  |  |
| 258 | create-an-object-at-a-given-address |  |  |  |
| 259 | csv-data-manipulation |  |  |  |
| 260 | csv-to-html-translation-1 |  |  |  |
| 261 | csv-to-html-translation-2 |  |  |  |
| 262 | csv-to-html-translation-3 |  |  |  |
| 263 | csv-to-html-translation-4 |  |  |  |
| 264 | csv-to-html-translation-5 |  |  |  |
| 265 | cuban-primes |  |  |  |
| 266 | cullen-and-woodall-numbers |  |  |  |
| 267 | cumulative-standard-deviation |  |  |  |
| 268 | currency |  |  |  |
| 269 | currying |  |  |  |
| 270 | curzon-numbers |  |  |  |
| 271 | cusip |  |  |  |
| 272 | cyclops-numbers |  |  |  |
| 273 | damm-algorithm |  |  |  |
| 274 | date-format |  |  |  |
| 275 | date-manipulation |  |  |  |
| 276 | day-of-the-week |  |  |  |
| 277 | de-bruijn-sequences |  |  |  |
| 278 | deal-cards-for-freecell |  |  |  |
| 279 | death-star |  |  |  |
| 280 | deceptive-numbers |  |  |  |
| 281 | deconvolution-1d-2 |  |  |  |
| 282 | deconvolution-1d-3 |  |  |  |
| 283 | deconvolution-1d |  |  |  |
| 284 | deepcopy-1 |  |  |  |
| 285 | define-a-primitive-data-type |  |  |  |
| 286 | delegates |  |  |  |
| 287 | demings-funnel |  |  |  |
| 288 | department-numbers |  |  |  |
| 289 | descending-primes |  |  |  |
| 290 | detect-division-by-zero |  |  |  |
| 291 | determine-if-a-string-has-all-the-same-characters |  |  |  |
| 292 | determine-if-a-string-has-all-unique-characters |  |  |  |
| 293 | determine-if-a-string-is-collapsible |  |  |  |
| 294 | determine-if-a-string-is-numeric-1 |  |  |  |
| 295 | determine-if-a-string-is-numeric-2 |  |  |  |
| 296 | determine-if-a-string-is-squeezable |  |  |  |
| 297 | determine-if-only-one-instance-is-running |  |  |  |
| 298 | determine-if-two-triangles-overlap |  |  |  |
| 299 | determine-sentence-type |  |  |  |
| 300 | dice-game-probabilities-1 |  |  |  |
| 301 | dice-game-probabilities-2 |  |  |  |
| 302 | digital-root-multiplicative-digital-root |  |  |  |
| 303 | dijkstras-algorithm |  |  |  |
| 304 | dinesmans-multiple-dwelling-problem |  |  |  |
| 305 | dining-philosophers-1 |  |  |  |
| 306 | dining-philosophers-2 |  |  |  |
| 307 | disarium-numbers |  |  |  |
| 308 | discordian-date |  |  |  |
| 309 | display-a-linear-combination |  |  |  |
| 310 | display-an-outline-as-a-nested-table |  |  |  |
| 311 | distance-and-bearing |  |  |  |
| 312 | distributed-programming |  |  |  |
| 313 | diversity-prediction-theorem |  |  |  |
| 314 | documentation |  |  |  |
| 315 | doomsday-rule |  |  |  |
| 316 | dot-product |  |  |  |
| 317 | doubly-linked-list-definition-1 |  |  |  |
| 318 | doubly-linked-list-definition-2 |  |  |  |
| 319 | doubly-linked-list-element-definition |  |  |  |
| 320 | doubly-linked-list-traversal |  |  |  |
| 321 | dragon-curve |  |  |  |
| 322 | draw-a-clock |  |  |  |
| 323 | draw-a-cuboid |  |  |  |
| 324 | draw-a-pixel-1 |  |  |  |
| 325 | draw-a-rotating-cube |  |  |  |
| 326 | draw-a-sphere |  |  |  |
| 327 | dutch-national-flag-problem |  |  |  |
| 328 | dynamic-variable-names |  |  |  |
| 329 | earliest-difference-between-prime-gaps |  |  |  |
| 330 | eban-numbers |  |  |  |
| 331 | ecdsa-example |  |  |  |
| 332 | echo-server |  |  |  |
| 333 | eertree |  |  |  |
| 334 | egyptian-division |  |  |  |
| 335 | ekg-sequence-convergence |  |  |  |
| 336 | element-wise-operations |  |  |  |
| 337 | elementary-cellular-automaton-infinite-length |  |  |  |
| 338 | elementary-cellular-automaton-random-number-generator |  |  |  |
| 339 | elementary-cellular-automaton |  |  |  |
| 340 | elliptic-curve-arithmetic |  |  |  |
| 341 | elliptic-curve-digital-signature-algorithm |  |  |  |
| 342 | emirp-primes |  |  |  |
| 343 | empty-directory |  |  |  |
| 344 | empty-program |  |  |  |
| 345 | empty-string-1 |  |  |  |
| 346 | empty-string-2 |  |  |  |
| 347 | enforced-immutability |  |  |  |
| 348 | entropy-1 |  |  |  |
| 349 | entropy-2 |  |  |  |
| 350 | entropy-narcissist |  |  |  |
| 351 | enumerations-1 |  |  |  |
| 352 | enumerations-2 |  |  |  |
| 353 | enumerations-3 |  |  |  |
| 354 | enumerations-4 |  |  |  |
| 355 | environment-variables-1 |  |  |  |
| 356 | environment-variables-2 |  |  |  |
| 357 | equal-prime-and-composite-sums |  |  |  |
| 358 | equilibrium-index |  |  |  |
| 359 | erd-s-nicolas-numbers |  |  |  |
| 360 | erd-s-selfridge-categorization-of-primes |  |  |  |
| 361 | esthetic-numbers |  |  |  |
| 362 | ethiopian-multiplication |  |  |  |
| 363 | euclid-mullin-sequence |  |  |  |
| 364 | euler-method |  |  |  |
| 365 | eulers-constant-0.5772... |  |  |  |
| 366 | eulers-identity |  |  |  |
| 367 | eulers-sum-of-powers-conjecture |  |  |  |
| 368 | evaluate-binomial-coefficients |  |  |  |
| 369 | even-or-odd |  |  |  |
| 370 | events |  |  |  |
| 371 | evolutionary-algorithm |  |  |  |
| 372 | exceptions-catch-an-exception-thrown-in-a-nested-call |  |  |  |
| 373 | exceptions |  |  |  |
| 374 | executable-library |  |  |  |
| 375 | execute-a-markov-algorithm |  |  |  |
| 376 | execute-a-system-command |  |  |  |
| 377 | execute-brain- |  |  |  |
| 378 | execute-computer-zero-1 |  |  |  |
| 379 | execute-computer-zero |  |  |  |
| 380 | execute-hq9+ |  |  |  |
| 381 | execute-snusp |  |  |  |
| 382 | exponentiation-operator-2 |  |  |  |
| 383 | exponentiation-operator |  |  |  |
| 384 | exponentiation-order |  |  |  |
| 385 | exponentiation-with-infix-operators-in-or-operating-on-the-base |  |  |  |
| 386 | extend-your-language |  |  |  |
| 387 | extensible-prime-generator |  |  |  |
| 388 | extreme-floating-point-values |  |  |  |
| 389 | faces-from-a-mesh-2 |  |  |  |
| 390 | faces-from-a-mesh |  |  |  |
| 391 | factorial-base-numbers-indexing-permutations-of-a-collection |  |  |  |
| 392 | factorial-primes |  |  |  |
| 393 | factorial |  |  |  |
| 394 | factorions |  |  |  |
| 395 | factors-of-a-mersenne-number |  |  |  |
| 396 | factors-of-an-integer |  |  |  |
| 397 | fairshare-between-two-and-more |  |  |  |
| 398 | farey-sequence |  |  |  |
| 399 | fast-fourier-transform |  |  |  |
| 400 | fasta-format |  |  |  |
| 401 | faulhabers-formula |  |  |  |
| 402 | faulhabers-triangle |  |  |  |
| 403 | feigenbaum-constant-calculation |  |  |  |
| 404 | fermat-numbers |  |  |  |
| 405 | fibonacci-n-step-number-sequences |  |  |  |
| 406 | fibonacci-sequence-1 |  |  |  |
| 407 | fibonacci-sequence-2 |  |  |  |
| 408 | fibonacci-sequence-3 |  |  |  |
| 409 | fibonacci-sequence-4 |  |  |  |
| 410 | fibonacci-sequence-5 |  |  |  |
| 411 | fibonacci-word-fractal |  |  |  |
| 412 | fibonacci-word |  |  |  |
| 413 | file-extension-is-in-extensions-list |  |  |  |
| 414 | file-input-output-1 |  |  |  |
| 415 | file-input-output-2 |  |  |  |
| 416 | file-input-output-3 |  |  |  |
| 417 | file-modification-time |  |  |  |
| 418 | file-size-distribution |  |  |  |
| 419 | file-size |  |  |  |
| 420 | filter |  |  |  |
| 421 | find-chess960-starting-position-identifier-2 |  |  |  |
| 422 | find-chess960-starting-position-identifier |  |  |  |
| 423 | find-common-directory-path |  |  |  |
| 424 | find-duplicate-files |  |  |  |
| 425 | find-if-a-point-is-within-a-triangle |  |  |  |
| 426 | find-largest-left-truncatable-prime-in-a-given-base |  |  |  |
| 427 | find-limit-of-recursion |  |  |  |
| 428 | find-palindromic-numbers-in-both-binary-and-ternary-bases |  |  |  |
| 429 | find-the-intersection-of-a-line-with-a-plane |  |  |  |
| 430 | find-the-intersection-of-two-lines |  |  |  |
| 431 | find-the-last-sunday-of-each-month |  |  |  |
| 432 | find-the-missing-permutation |  |  |  |
| 433 | first-class-environments |  |  |  |
| 434 | first-class-functions-use-numbers-analogously |  |  |  |
| 435 | first-power-of-2-that-has-leading-decimal-digits-of-12 |  |  |  |
| 436 | five-weekends |  |  |  |
| 437 | fivenum-1 |  |  |  |
| 438 | fivenum-2 |  |  |  |
| 439 | fivenum-3 |  |  |  |
| 440 | fixed-length-records-1 |  |  |  |
| 441 | fixed-length-records-2 |  |  |  |
| 442 | fizzbuzz-1 |  |  |  |
| 443 | fizzbuzz-2 |  |  |  |
| 444 | fizzbuzz |  |  |  |
| 445 | flatten-a-list-1 |  |  |  |
| 446 | flatten-a-list-2 |  |  |  |
| 447 | flipping-bits-game |  |  |  |
| 448 | flow-control-structures-1 |  |  |  |
| 449 | flow-control-structures-2 |  |  |  |
| 450 | flow-control-structures-3 |  |  |  |
| 451 | flow-control-structures-4 |  |  |  |
| 452 | floyd-warshall-algorithm |  |  |  |
| 453 | floyd-warshall-algorithm2 |  |  |  |
| 454 | floyds-triangle |  |  |  |
| 455 | forest-fire |  |  |  |
| 456 | fork-2 |  |  |  |
| 457 | fork |  |  |  |
| 458 | formal-power-series |  |  |  |
| 459 | formatted-numeric-output |  |  |  |
| 460 | forward-difference |  |  |  |
| 461 | four-bit-adder-1 |  |  |  |
| 462 | four-is-magic |  |  |  |
| 463 | four-is-the-number-of-letters-in-the-... |  |  |  |
| 464 | fractal-tree |  |  |  |
| 465 | fractran |  |  |  |
| 466 | french-republican-calendar |  |  |  |
| 467 | ftp |  |  |  |
| 468 | function-frequency |  |  |  |
| 469 | function-prototype |  |  |  |
| 470 | functional-coverage-tree |  |  |  |
| 471 | fusc-sequence |  |  |  |
| 472 | gamma-function |  |  |  |
| 473 | general-fizzbuzz |  |  |  |
| 474 | generic-swap |  |  |  |
| 475 | get-system-command-output |  |  |  |
| 476 | giuga-numbers |  |  |  |
| 477 | globally-replace-text-in-several-files |  |  |  |
| 478 | goldbachs-comet |  |  |  |
| 479 | golden-ratio-convergence |  |  |  |
| 480 | graph-colouring |  |  |  |
| 481 | gray-code |  |  |  |
| 482 | gui-component-interaction |  |  |  |
| 483 | gui-enabling-disabling-of-controls |  |  |  |
| 484 | gui-maximum-window-dimensions |  |  |  |
| 485 | http |  |  |  |
| 486 | image-noise |  |  |  |
| 487 | loops-increment-loop-index-within-loop-body |  |  |  |
| 488 | md5 |  |  |  |
| 489 | nim-game |  |  |  |
| 490 | plasma-effect |  |  |  |
| 491 | sorting-algorithms-bubble-sort |  |  |  |
| 492 | window-management |  |  |  |
| 493 | zumkeller-numbers |  |  |  |
