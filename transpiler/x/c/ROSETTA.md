# C Transpiler Rosetta Output

This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (60/452) - Last updated 2025-07-27 11:07 +0700:
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
| 17 | a+b | ✓ | 30us | 8.7 KB |
| 18 | abbreviations-automatic | ✓ | 846us | 536.0 KB |
| 19 | abbreviations-easy | ✓ | 246us | 108.3 KB |
| 20 | abbreviations-simple | ✓ | 402us | 140.8 KB |
| 21 | abc-problem | ✓ | 238us | 69.5 KB |
| 22 | abelian-sandpile-model-identity | ✓ | 387us | 24.6 KB |
| 23 | abelian-sandpile-model | ✓ | 451us | 15.2 KB |
| 24 | abstract-type |  |  |  |
| 25 | abundant-deficient-and-perfect-number-classifications | ✓ | 317.38ms | 5.0 KB |
| 26 | abundant-odd-numbers |  |  |  |
| 27 | accumulator-factory |  |  |  |
| 28 | achilles-numbers |  |  |  |
| 29 | ackermann-function-2 | ✓ | 35us | 4.9 KB |
| 30 | ackermann-function-3 | ✓ | 41us | 6.7 KB |
| 31 | ackermann-function | ✓ | 112us | 4.9 KB |
| 32 | active-directory-connect | ✓ | 24us | 4.7 KB |
| 33 | active-directory-search-for-a-user | ✓ | 25us | 5.0 KB |
| 34 | active-object | ✓ | 124us | 4.7 KB |
| 35 | add-a-variable-to-a-class-instance-at-runtime |  |  |  |
| 36 | additive-primes | ✓ | 61us | 11.8 KB |
| 37 | address-of-a-variable | ✓ | 36us | 4.7 KB |
| 38 | adfgvx-cipher |  |  |  |
| 39 | aks-test-for-primes | ✓ | 65us | 12.7 KB |
| 40 | algebraic-data-types |  |  |  |
| 41 | align-columns |  |  |  |
| 42 | aliquot-sequence-classifications |  |  |  |
| 43 | almkvist-giullera-formula-for-pi |  |  |  |
| 44 | almost-prime | ✓ | 106us | 7.1 KB |
| 45 | amb | ✓ | 39us | 6.0 KB |
| 46 | amicable-pairs | ✓ | 224.39ms | 84.4 KB |
| 47 | anagrams-deranged-anagrams |  |  |  |
| 48 | anagrams |  |  |  |
| 49 | angle-difference-between-two-bearings-1 | ✓ | 21us | 4.7 KB |
| 50 | angle-difference-between-two-bearings-2 | ✓ | 21us | 4.7 KB |
| 51 | angles-geometric-normalization-and-conversion | ✓ | 128us | 25.5 KB |
| 52 | animate-a-pendulum | ✓ | 22us | 5.0 KB |
| 53 | animation | ✓ | 150us | 57.5 KB |
| 54 | anonymous-recursion-1 | ✓ | 29us | 5.9 KB |
| 55 | anonymous-recursion-2 | ✓ | 34us | 6.3 KB |
| 56 | anonymous-recursion | ✓ | 28us | 6.5 KB |
| 57 | anti-primes | ✓ | 28.61ms | 7.3 KB |
| 58 | append-a-record-to-the-end-of-a-text-file | ✓ | 16us | 4.7 KB |
| 59 | apply-a-callback-to-an-array-1 | ✓ | 357us | 4.8 KB |
| 60 | apply-a-callback-to-an-array-2 |  |  |  |
| 61 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 87us | 1.3 KB |
| 62 | approximate-equality | ✓ | 42us | 6.3 KB |
| 63 | arbitrary-precision-integers-included- | ✓ | 19us | 4.8 KB |
| 64 | archimedean-spiral | ✓ | 819us | 13.8 KB |
| 65 | arena-storage-pool | ✓ | 272us | 4.9 KB |
| 66 | arithmetic-complex | ✓ | 412us | 6.1 KB |
| 67 | arithmetic-derivative | ✓ | 946us | 79.3 KB |
| 68 | arithmetic-evaluation | ✓ | 710us | 6.9 KB |
| 69 | arithmetic-geometric-mean-calculate-pi | ✓ | 275us | 4.7 KB |
| 70 | arithmetic-geometric-mean | ✓ | 284us | 4.7 KB |
| 71 | arithmetic-integer-1 | ✓ | 220us | 5.8 KB |
| 72 | arithmetic-integer-2 | ✓ | 392us | 5.8 KB |
| 73 | arithmetic-numbers |  |  |  |
| 74 | arithmetic-rational | ✓ | 899us | 7.3 KB |
| 75 | array-concatenation |  |  |  |
| 76 | array-length | ✓ | 152us | 4.9 KB |
| 77 | arrays | ✓ | 358us | 12.0 KB |
| 78 | ascending-primes | ✓ | 99us | 34.5 KB |
| 79 | ascii-art-diagram-converter | ✓ | 18us | 4.7 KB |
| 80 | assertions | ✓ | 13us | 4.7 KB |
| 81 | associative-array-creation |  |  |  |
| 82 | associative-array-iteration |  |  |  |
| 83 | associative-array-merging |  |  |  |
| 84 | atomic-updates |  |  |  |
| 85 | attractive-numbers |  |  |  |
| 86 | average-loop-length |  |  |  |
| 87 | averages-arithmetic-mean |  |  |  |
| 88 | averages-mean-time-of-day |  |  |  |
| 89 | averages-median-1 |  |  |  |
| 90 | averages-median-2 |  |  |  |
| 91 | averages-median-3 |  |  |  |
| 92 | averages-mode |  |  |  |
| 93 | averages-pythagorean-means |  |  |  |
| 94 | averages-root-mean-square |  |  |  |
| 95 | averages-simple-moving-average |  |  |  |
| 96 | avl-tree |  |  |  |
| 97 | b-zier-curves-intersections |  |  |  |
| 98 | babbage-problem |  |  |  |
| 99 | babylonian-spiral |  |  |  |
| 100 | balanced-brackets |  |  |  |
| 101 | balanced-ternary |  |  |  |
| 102 | barnsley-fern |  |  |  |
| 103 | base64-decode-data |  |  |  |
| 104 | bell-numbers |  |  |  |
| 105 | benfords-law |  |  |  |
| 106 | bernoulli-numbers |  |  |  |
| 107 | best-shuffle |  |  |  |
| 108 | bifid-cipher |  |  |  |
| 109 | bin-given-limits |  |  |  |
| 110 | binary-digits |  |  |  |
| 111 | binary-search |  |  |  |
| 112 | binary-strings |  |  |  |
| 113 | bioinformatics-base-count |  |  |  |
| 114 | bioinformatics-global-alignment |  |  |  |
| 115 | bioinformatics-sequence-mutation |  |  |  |
| 116 | biorhythms |  |  |  |
| 117 | bitcoin-address-validation |  |  |  |
| 118 | bitmap-b-zier-curves-cubic |  |  |  |
| 119 | bitmap-b-zier-curves-quadratic |  |  |  |
| 120 | bitmap-bresenhams-line-algorithm |  |  |  |
| 121 | bitmap-flood-fill |  |  |  |
| 122 | bitmap-histogram |  |  |  |
| 123 | bitmap-midpoint-circle-algorithm |  |  |  |
| 124 | bitmap-ppm-conversion-through-a-pipe |  |  |  |
| 125 | bitmap-read-a-ppm-file |  |  |  |
| 126 | bitmap-read-an-image-through-a-pipe |  |  |  |
| 127 | bitmap-write-a-ppm-file |  |  |  |
| 128 | bitmap |  |  |  |
| 129 | bitwise-io-1 |  |  |  |
| 130 | bitwise-io-2 |  |  |  |
| 131 | bitwise-operations |  |  |  |
| 132 | blum-integer |  |  |  |
| 133 | boolean-values |  |  |  |
| 134 | box-the-compass |  |  |  |
| 135 | boyer-moore-string-search |  |  |  |
| 136 | brazilian-numbers |  |  |  |
| 137 | break-oo-privacy |  |  |  |
| 138 | brilliant-numbers |  |  |  |
| 139 | brownian-tree |  |  |  |
| 140 | bulls-and-cows-player |  |  |  |
| 141 | bulls-and-cows |  |  |  |
| 142 | burrows-wheeler-transform |  |  |  |
| 143 | caesar-cipher-1 |  |  |  |
| 144 | caesar-cipher-2 |  |  |  |
| 145 | calculating-the-value-of-e |  |  |  |
| 146 | calendar---for-real-programmers-1 |  |  |  |
| 147 | calendar---for-real-programmers-2 |  |  |  |
| 148 | calendar |  |  |  |
| 149 | calkin-wilf-sequence |  |  |  |
| 150 | call-a-foreign-language-function |  |  |  |
| 151 | call-a-function-1 |  |  |  |
| 152 | call-a-function-10 |  |  |  |
| 153 | call-a-function-11 |  |  |  |
| 154 | call-a-function-12 |  |  |  |
| 155 | call-a-function-2 |  |  |  |
| 156 | call-a-function-3 |  |  |  |
| 157 | call-a-function-4 |  |  |  |
| 158 | call-a-function-5 |  |  |  |
| 159 | call-a-function-6 |  |  |  |
| 160 | call-a-function-7 |  |  |  |
| 161 | call-a-function-8 |  |  |  |
| 162 | call-a-function-9 |  |  |  |
| 163 | call-an-object-method-1 |  |  |  |
| 164 | call-an-object-method-2 |  |  |  |
| 165 | call-an-object-method-3 |  |  |  |
| 166 | call-an-object-method |  |  |  |
| 167 | camel-case-and-snake-case |  |  |  |
| 168 | canny-edge-detector |  |  |  |
| 169 | canonicalize-cidr |  |  |  |
| 170 | cantor-set |  |  |  |
| 171 | carmichael-3-strong-pseudoprimes |  |  |  |
| 172 | cartesian-product-of-two-or-more-lists-1 |  |  |  |
| 173 | cartesian-product-of-two-or-more-lists-2 |  |  |  |
| 174 | cartesian-product-of-two-or-more-lists-3 |  |  |  |
| 175 | cartesian-product-of-two-or-more-lists-4 |  |  |  |
| 176 | case-sensitivity-of-identifiers |  |  |  |
| 177 | casting-out-nines |  |  |  |
| 178 | catalan-numbers-1 |  |  |  |
| 179 | catalan-numbers-2 |  |  |  |
| 180 | catalan-numbers-pascals-triangle |  |  |  |
| 181 | catamorphism |  |  |  |
| 182 | catmull-clark-subdivision-surface |  |  |  |
| 183 | chaocipher |  |  |  |
| 184 | chaos-game |  |  |  |
| 185 | character-codes-1 |  |  |  |
| 186 | character-codes-2 |  |  |  |
| 187 | character-codes-3 |  |  |  |
| 188 | character-codes-4 |  |  |  |
| 189 | character-codes-5 |  |  |  |
| 190 | chat-server |  |  |  |
| 191 | check-machin-like-formulas |  |  |  |
| 192 | check-that-file-exists |  |  |  |
| 193 | checkpoint-synchronization-1 |  |  |  |
| 194 | checkpoint-synchronization-2 |  |  |  |
| 195 | checkpoint-synchronization-3 |  |  |  |
| 196 | checkpoint-synchronization-4 |  |  |  |
| 197 | chernicks-carmichael-numbers |  |  |  |
| 198 | cheryls-birthday |  |  |  |
| 199 | chinese-remainder-theorem |  |  |  |
| 200 | chinese-zodiac |  |  |  |
| 201 | cholesky-decomposition-1 |  |  |  |
| 202 | cholesky-decomposition |  |  |  |
| 203 | chowla-numbers |  |  |  |
| 204 | church-numerals-1 |  |  |  |
| 205 | church-numerals-2 |  |  |  |
| 206 | circles-of-given-radius-through-two-points |  |  |  |
| 207 | circular-primes |  |  |  |
| 208 | cistercian-numerals |  |  |  |
| 209 | comma-quibbling |  |  |  |
| 210 | compiler-virtual-machine-interpreter |  |  |  |
| 211 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k |  |  |  |
| 212 | compound-data-type |  |  |  |
| 213 | concurrent-computing-1 |  |  |  |
| 214 | concurrent-computing-2 |  |  |  |
| 215 | concurrent-computing-3 |  |  |  |
| 216 | conditional-structures-1 |  |  |  |
| 217 | conditional-structures-10 |  |  |  |
| 218 | conditional-structures-2 |  |  |  |
| 219 | conditional-structures-3 |  |  |  |
| 220 | conditional-structures-4 |  |  |  |
| 221 | conditional-structures-5 |  |  |  |
| 222 | conditional-structures-6 |  |  |  |
| 223 | conditional-structures-7 |  |  |  |
| 224 | conditional-structures-8 |  |  |  |
| 225 | conditional-structures-9 |  |  |  |
| 226 | consecutive-primes-with-ascending-or-descending-differences |  |  |  |
| 227 | constrained-genericity-1 |  |  |  |
| 228 | constrained-genericity-2 |  |  |  |
| 229 | constrained-genericity-3 |  |  |  |
| 230 | constrained-genericity-4 |  |  |  |
| 231 | constrained-random-points-on-a-circle-1 |  |  |  |
| 232 | constrained-random-points-on-a-circle-2 |  |  |  |
| 233 | continued-fraction |  |  |  |
| 234 | convert-decimal-number-to-rational |  |  |  |
| 235 | convert-seconds-to-compound-duration |  |  |  |
| 236 | convex-hull |  |  |  |
| 237 | conways-game-of-life |  |  |  |
| 238 | copy-a-string-1 |  |  |  |
| 239 | copy-a-string-2 |  |  |  |
| 240 | copy-stdin-to-stdout-1 |  |  |  |
| 241 | copy-stdin-to-stdout-2 |  |  |  |
| 242 | count-in-factors |  |  |  |
| 243 | count-in-octal-1 |  |  |  |
| 244 | count-in-octal-2 |  |  |  |
| 245 | count-in-octal-3 |  |  |  |
| 246 | count-in-octal-4 |  |  |  |
| 247 | count-occurrences-of-a-substring |  |  |  |
| 248 | count-the-coins-1 |  |  |  |
| 249 | count-the-coins-2 |  |  |  |
| 250 | cramers-rule |  |  |  |
| 251 | crc-32-1 |  |  |  |
| 252 | crc-32-2 |  |  |  |
| 253 | create-a-file-on-magnetic-tape |  |  |  |
| 254 | create-a-file |  |  |  |
| 255 | create-a-two-dimensional-array-at-runtime-1 |  |  |  |
| 256 | create-an-html-table |  |  |  |
| 257 | create-an-object-at-a-given-address |  |  |  |
| 258 | csv-data-manipulation |  |  |  |
| 259 | csv-to-html-translation-1 |  |  |  |
| 260 | csv-to-html-translation-2 |  |  |  |
| 261 | csv-to-html-translation-3 |  |  |  |
| 262 | csv-to-html-translation-4 |  |  |  |
| 263 | csv-to-html-translation-5 |  |  |  |
| 264 | cuban-primes |  |  |  |
| 265 | cullen-and-woodall-numbers |  |  |  |
| 266 | cumulative-standard-deviation |  |  |  |
| 267 | currency |  |  |  |
| 268 | currying |  |  |  |
| 269 | curzon-numbers |  |  |  |
| 270 | cusip |  |  |  |
| 271 | cyclops-numbers |  |  |  |
| 272 | damm-algorithm |  |  |  |
| 273 | date-format |  |  |  |
| 274 | date-manipulation |  |  |  |
| 275 | day-of-the-week |  |  |  |
| 276 | de-bruijn-sequences |  |  |  |
| 277 | deal-cards-for-freecell |  |  |  |
| 278 | death-star |  |  |  |
| 279 | deceptive-numbers |  |  |  |
| 280 | deconvolution-1d-2 |  |  |  |
| 281 | deconvolution-1d-3 |  |  |  |
| 282 | deconvolution-1d |  |  |  |
| 283 | deepcopy-1 |  |  |  |
| 284 | define-a-primitive-data-type |  |  |  |
| 285 | delegates |  |  |  |
| 286 | demings-funnel |  |  |  |
| 287 | department-numbers |  |  |  |
| 288 | descending-primes |  |  |  |
| 289 | detect-division-by-zero |  |  |  |
| 290 | determine-if-a-string-has-all-the-same-characters |  |  |  |
| 291 | determine-if-a-string-has-all-unique-characters |  |  |  |
| 292 | determine-if-a-string-is-collapsible |  |  |  |
| 293 | determine-if-a-string-is-numeric-1 |  |  |  |
| 294 | determine-if-a-string-is-numeric-2 |  |  |  |
| 295 | determine-if-a-string-is-squeezable |  |  |  |
| 296 | determine-if-only-one-instance-is-running |  |  |  |
| 297 | determine-if-two-triangles-overlap |  |  |  |
| 298 | determine-sentence-type |  |  |  |
| 299 | dice-game-probabilities-1 |  |  |  |
| 300 | dice-game-probabilities-2 |  |  |  |
| 301 | digital-root-multiplicative-digital-root |  |  |  |
| 302 | dijkstras-algorithm |  |  |  |
| 303 | dinesmans-multiple-dwelling-problem |  |  |  |
| 304 | dining-philosophers-1 |  |  |  |
| 305 | dining-philosophers-2 |  |  |  |
| 306 | disarium-numbers |  |  |  |
| 307 | discordian-date |  |  |  |
| 308 | display-a-linear-combination |  |  |  |
| 309 | display-an-outline-as-a-nested-table |  |  |  |
| 310 | distance-and-bearing |  |  |  |
| 311 | distributed-programming |  |  |  |
| 312 | diversity-prediction-theorem |  |  |  |
| 313 | documentation |  |  |  |
| 314 | doomsday-rule |  |  |  |
| 315 | dot-product |  |  |  |
| 316 | doubly-linked-list-definition-1 |  |  |  |
| 317 | doubly-linked-list-definition-2 |  |  |  |
| 318 | doubly-linked-list-element-definition |  |  |  |
| 319 | doubly-linked-list-traversal |  |  |  |
| 320 | dragon-curve |  |  |  |
| 321 | draw-a-clock |  |  |  |
| 322 | draw-a-cuboid |  |  |  |
| 323 | draw-a-pixel-1 |  |  |  |
| 324 | draw-a-rotating-cube |  |  |  |
| 325 | draw-a-sphere |  |  |  |
| 326 | dutch-national-flag-problem |  |  |  |
| 327 | dynamic-variable-names |  |  |  |
| 328 | earliest-difference-between-prime-gaps |  |  |  |
| 329 | eban-numbers |  |  |  |
| 330 | ecdsa-example |  |  |  |
| 331 | echo-server |  |  |  |
| 332 | eertree |  |  |  |
| 333 | egyptian-division |  |  |  |
| 334 | ekg-sequence-convergence |  |  |  |
| 335 | element-wise-operations |  |  |  |
| 336 | elementary-cellular-automaton-infinite-length |  |  |  |
| 337 | elementary-cellular-automaton-random-number-generator |  |  |  |
| 338 | elementary-cellular-automaton |  |  |  |
| 339 | elliptic-curve-arithmetic |  |  |  |
| 340 | elliptic-curve-digital-signature-algorithm |  |  |  |
| 341 | emirp-primes |  |  |  |
| 342 | empty-directory |  |  |  |
| 343 | empty-program |  |  |  |
| 344 | empty-string-1 |  |  |  |
| 345 | empty-string-2 |  |  |  |
| 346 | enforced-immutability |  |  |  |
| 347 | entropy-1 |  |  |  |
| 348 | entropy-2 |  |  |  |
| 349 | entropy-narcissist |  |  |  |
| 350 | enumerations-1 |  |  |  |
| 351 | enumerations-2 |  |  |  |
| 352 | enumerations-3 |  |  |  |
| 353 | enumerations-4 |  |  |  |
| 354 | environment-variables-1 |  |  |  |
| 355 | environment-variables-2 |  |  |  |
| 356 | equal-prime-and-composite-sums |  |  |  |
| 357 | equilibrium-index |  |  |  |
| 358 | erd-s-nicolas-numbers |  |  |  |
| 359 | erd-s-selfridge-categorization-of-primes |  |  |  |
| 360 | esthetic-numbers |  |  |  |
| 361 | ethiopian-multiplication |  |  |  |
| 362 | euclid-mullin-sequence |  |  |  |
| 363 | euler-method |  |  |  |
| 364 | eulers-constant-0.5772... |  |  |  |
| 365 | eulers-identity |  |  |  |
| 366 | eulers-sum-of-powers-conjecture |  |  |  |
| 367 | evaluate-binomial-coefficients |  |  |  |
| 368 | even-or-odd |  |  |  |
| 369 | events |  |  |  |
| 370 | evolutionary-algorithm |  |  |  |
| 371 | exceptions-catch-an-exception-thrown-in-a-nested-call |  |  |  |
| 372 | exceptions |  |  |  |
| 373 | executable-library |  |  |  |
| 374 | execute-a-markov-algorithm |  |  |  |
| 375 | execute-a-system-command |  |  |  |
| 376 | execute-brain- |  |  |  |
| 377 | execute-computer-zero |  |  |  |
| 378 | execute-hq9+ |  |  |  |
| 379 | execute-snusp |  |  |  |
| 380 | exponentiation-operator |  |  |  |
| 381 | exponentiation-order |  |  |  |
| 382 | exponentiation-with-infix-operators-in-or-operating-on-the-base |  |  |  |
| 383 | extend-your-language |  |  |  |
| 384 | extensible-prime-generator |  |  |  |
| 385 | extreme-floating-point-values |  |  |  |
| 386 | faces-from-a-mesh |  |  |  |
| 387 | fasta-format |  |  |  |
| 388 | faulhabers-triangle |  |  |  |
| 389 | feigenbaum-constant-calculation |  |  |  |
| 390 | fermat-numbers |  |  |  |
| 391 | fibonacci-n-step-number-sequences |  |  |  |
| 392 | fibonacci-sequence-1 |  |  |  |
| 393 | fibonacci-sequence-2 |  |  |  |
| 394 | fibonacci-sequence-3 |  |  |  |
| 395 | fibonacci-sequence-4 |  |  |  |
| 396 | fibonacci-word-fractal |  |  |  |
| 397 | fibonacci-word |  |  |  |
| 398 | file-extension-is-in-extensions-list |  |  |  |
| 399 | file-input-output-1 |  |  |  |
| 400 | file-input-output-2 |  |  |  |
| 401 | file-modification-time |  |  |  |
| 402 | file-size-distribution |  |  |  |
| 403 | file-size |  |  |  |
| 404 | filter |  |  |  |
| 405 | find-chess960-starting-position-identifier |  |  |  |
| 406 | find-common-directory-path |  |  |  |
| 407 | find-duplicate-files |  |  |  |
| 408 | find-if-a-point-is-within-a-triangle |  |  |  |
| 409 | find-largest-left-truncatable-prime-in-a-given-base |  |  |  |
| 410 | find-limit-of-recursion |  |  |  |
| 411 | find-palindromic-numbers-in-both-binary-and-ternary-bases |  |  |  |
| 412 | find-the-intersection-of-a-line-with-a-plane |  |  |  |
| 413 | find-the-intersection-of-two-lines |  |  |  |
| 414 | find-the-last-sunday-of-each-month |  |  |  |
| 415 | find-the-missing-permutation |  |  |  |
| 416 | fivenum-1 |  |  |  |
| 417 | fivenum-2 |  |  |  |
| 418 | fixed-length-records-1 |  |  |  |
| 419 | fixed-length-records-2 |  |  |  |
| 420 | fizzbuzz-1 |  |  |  |
| 421 | fizzbuzz-2 |  |  |  |
| 422 | flatten-a-list-1 |  |  |  |
| 423 | flatten-a-list-2 |  |  |  |
| 424 | flipping-bits-game |  |  |  |
| 425 | flow-control-structures-1 |  |  |  |
| 426 | flow-control-structures-2 |  |  |  |
| 427 | flow-control-structures-3 |  |  |  |
| 428 | flow-control-structures-4 |  |  |  |
| 429 | floyd-warshall-algorithm |  |  |  |
| 430 | floyds-triangle |  |  |  |
| 431 | forest-fire |  |  |  |
| 432 | fork |  |  |  |
| 433 | ftp |  |  |  |
| 434 | gamma-function |  |  |  |
| 435 | general-fizzbuzz |  |  |  |
| 436 | generic-swap |  |  |  |
| 437 | get-system-command-output |  |  |  |
| 438 | giuga-numbers |  |  |  |
| 439 | globally-replace-text-in-several-files |  |  |  |
| 440 | goldbachs-comet |  |  |  |
| 441 | golden-ratio-convergence |  |  |  |
| 442 | graph-colouring |  |  |  |
| 443 | gray-code |  |  |  |
| 444 | http |  |  |  |
| 445 | image-noise |  |  |  |
| 446 | loops-increment-loop-index-within-loop-body |  |  |  |
| 447 | md5 |  |  |  |
| 448 | nim-game |  |  |  |
| 449 | plasma-effect |  |  |  |
| 450 | sorting-algorithms-bubble-sort |  |  |  |
| 451 | window-management |  |  |  |
| 452 | zumkeller-numbers |  |  |  |
