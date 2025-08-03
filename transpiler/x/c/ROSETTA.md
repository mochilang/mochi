# C Transpiler Rosetta Output

This directory stores C code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (142/491) - Last updated 2025-08-03 17:40 +0700:
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
| 15 | a+b | ✓ | 30us | 8.7 KB |
| 16 | abbreviations-automatic | ✓ | 846us | 536.0 KB |
| 17 | abbreviations-easy | ✓ | 246us | 108.3 KB |
| 18 | abbreviations-simple | ✓ | 402us | 140.8 KB |
| 19 | abc-problem | ✓ | 238us | 69.5 KB |
| 20 | abelian-sandpile-model-identity | ✓ | 387us | 24.6 KB |
| 21 | abelian-sandpile-model | ✓ | 451us | 15.2 KB |
| 22 | abstract-type |  |  |  |
| 23 | abundant-deficient-and-perfect-number-classifications | ✓ | 317.38ms | 5.0 KB |
| 24 | abundant-odd-numbers |  |  |  |
| 25 | accumulator-factory |  |  |  |
| 26 | achilles-numbers |  |  |  |
| 27 | ackermann-function-2 | ✓ | 35us | 4.9 KB |
| 28 | ackermann-function-3 | ✓ | 41us | 6.7 KB |
| 29 | ackermann-function | ✓ | 112us | 4.9 KB |
| 30 | active-directory-connect | ✓ | 24us | 4.7 KB |
| 31 | active-directory-search-for-a-user | ✓ | 25us | 5.0 KB |
| 32 | active-object | ✓ | 124us | 4.7 KB |
| 33 | add-a-variable-to-a-class-instance-at-runtime |  |  |  |
| 34 | additive-primes | ✓ | 61us | 11.8 KB |
| 35 | address-of-a-variable | ✓ | 36us | 4.7 KB |
| 36 | adfgvx-cipher |  |  |  |
| 37 | aks-test-for-primes | ✓ | 65us | 12.7 KB |
| 38 | algebraic-data-types |  |  |  |
| 39 | align-columns |  |  |  |
| 40 | aliquot-sequence-classifications |  |  |  |
| 41 | almkvist-giullera-formula-for-pi |  |  |  |
| 42 | almost-prime | ✓ | 106us | 7.1 KB |
| 43 | amb | ✓ | 39us | 6.0 KB |
| 44 | amicable-pairs | ✓ | 224.39ms | 84.4 KB |
| 45 | anagrams-deranged-anagrams |  |  |  |
| 46 | anagrams |  |  |  |
| 47 | angle-difference-between-two-bearings-1 | ✓ | 21us | 4.7 KB |
| 48 | angle-difference-between-two-bearings-2 | ✓ | 21us | 4.7 KB |
| 49 | angles-geometric-normalization-and-conversion | ✓ | 128us | 25.5 KB |
| 50 | animate-a-pendulum | ✓ | 22us | 5.0 KB |
| 51 | animation | ✓ | 150us | 57.5 KB |
| 52 | anonymous-recursion-1 | ✓ | 29us | 5.9 KB |
| 53 | anonymous-recursion-2 | ✓ | 34us | 6.3 KB |
| 54 | anonymous-recursion | ✓ | 28us | 6.5 KB |
| 55 | anti-primes | ✓ | 28.61ms | 7.3 KB |
| 56 | append-a-record-to-the-end-of-a-text-file | ✓ | 16us | 4.7 KB |
| 57 | apply-a-callback-to-an-array-1 | ✓ | 357us | 4.8 KB |
| 58 | apply-a-callback-to-an-array-2 |  |  |  |
| 59 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 87us | 1.3 KB |
| 60 | approximate-equality | ✓ | 42us | 6.3 KB |
| 61 | arbitrary-precision-integers-included- | ✓ | 19us | 4.8 KB |
| 62 | archimedean-spiral | ✓ | 819us | 13.8 KB |
| 63 | arena-storage-pool | ✓ | 272us | 4.9 KB |
| 64 | arithmetic-complex | ✓ | 412us | 6.1 KB |
| 65 | arithmetic-derivative | ✓ | 946us | 79.3 KB |
| 66 | arithmetic-evaluation | ✓ | 710us | 6.9 KB |
| 67 | arithmetic-geometric-mean-calculate-pi | ✓ | 275us | 4.7 KB |
| 68 | arithmetic-geometric-mean | ✓ | 284us | 4.7 KB |
| 69 | arithmetic-integer-1 | ✓ | 220us | 5.8 KB |
| 70 | arithmetic-integer-2 | ✓ | 392us | 5.8 KB |
| 71 | arithmetic-numbers |  |  |  |
| 72 | arithmetic-rational | ✓ | 899us | 7.3 KB |
| 73 | array-concatenation |  |  |  |
| 74 | array-length | ✓ | 152us | 4.9 KB |
| 75 | arrays | ✓ | 358us | 12.0 KB |
| 76 | ascending-primes | ✓ | 99us | 34.5 KB |
| 77 | ascii-art-diagram-converter | ✓ | 18us | 4.7 KB |
| 78 | assertions | ✓ | 13us | 4.7 KB |
| 79 | associative-array-creation |  |  |  |
| 80 | associative-array-iteration | ✓ | 348us | 5.3 KB |
| 81 | associative-array-merging | ✓ | 78us | 4.8 KB |
| 82 | atomic-updates | ✓ | 45us | 8.4 KB |
| 83 | attractive-numbers | ✓ | 61us | 15.7 KB |
| 84 | average-loop-length |  |  |  |
| 85 | averages-arithmetic-mean |  |  |  |
| 86 | averages-mean-time-of-day |  |  |  |
| 87 | averages-median-1 |  |  |  |
| 88 | averages-median-2 | ✓ | 323us | 4.7 KB |
| 89 | averages-median-3 | ✓ | 848us | 4.9 KB |
| 90 | averages-mode | ✓ | 368us | 4.9 KB |
| 91 | averages-pythagorean-means | ✓ | 29us | 5.0 KB |
| 92 | averages-root-mean-square | ✓ | 32us | 4.7 KB |
| 93 | averages-simple-moving-average | ✓ | 70us | 10.3 KB |
| 94 | avl-tree |  |  |  |
| 95 | b-zier-curves-intersections |  |  |  |
| 96 | babbage-problem | ✓ | 79us | 4.9 KB |
| 97 | babylonian-spiral |  |  |  |
| 98 | balanced-brackets | ✓ | 199us | 10.2 KB |
| 99 | balanced-ternary | ✓ | 359us | 8.5 KB |
| 100 | barnsley-fern | ✓ | 3.69ms | 481.8 KB |
| 101 | base64-decode-data | ✓ | 625us | 80.9 KB |
| 102 | bell-numbers | ✓ | 97us | 24.1 KB |
| 103 | benfords-law | ✓ | 341us | 18.2 KB |
| 104 | bernoulli-numbers | ✓ | 54.21ms | 12.8 MB |
| 105 | best-shuffle |  |  |  |
| 106 | bifid-cipher |  |  |  |
| 107 | bin-given-limits | ✓ | 282us | 11.8 KB |
| 108 | binary-digits | ✓ | 292us | 7.7 KB |
| 109 | binary-search | ✓ | 255us | 4.9 KB |
| 110 | binary-strings | ✓ | 376us | 7.5 KB |
| 111 | bioinformatics-base-count | ✓ | 345us | 23.8 KB |
| 112 | bioinformatics-global-alignment |  |  |  |
| 113 | bioinformatics-sequence-mutation | ✓ | 597us | 63.1 KB |
| 114 | biorhythms | ✓ | 327us | 13.2 KB |
| 115 | bitcoin-address-validation | ✓ | 35.67ms | 66.2 KB |
| 116 | bitmap-b-zier-curves-cubic |  |  |  |
| 117 | bitmap-b-zier-curves-quadratic | ✓ | 14.79ms | 4.6 MB |
| 118 | bitmap-bresenhams-line-algorithm | ✓ | 319us | 6.0 KB |
| 119 | bitmap-flood-fill |  |  |  |
| 120 | bitmap-histogram | ✓ | 661us | 1.4 MB |
| 121 | bitmap-midpoint-circle-algorithm | ✓ | 404us | 1.7 MB |
| 122 | bitmap-ppm-conversion-through-a-pipe |  |  |  |
| 123 | bitmap-read-a-ppm-file |  |  |  |
| 124 | bitmap-read-an-image-through-a-pipe |  |  |  |
| 125 | bitmap-write-a-ppm-file |  |  |  |
| 126 | bitmap |  |  |  |
| 127 | bitwise-io-1 |  |  |  |
| 128 | bitwise-io-2 |  |  |  |
| 129 | bitwise-operations | ✓ | 639us | 11.0 KB |
| 130 | blum-integer | ✓ | 265us | 10.8 KB |
| 131 | boolean-values | ✓ | 619us | 4.7 KB |
| 132 | box-the-compass | ✓ | 415us | 27.7 KB |
| 133 | boyer-moore-string-search | ✓ | 234us | 16.8 KB |
| 134 | brazilian-numbers | ✓ | 3.80s | 8.7 KB |
| 135 | break-oo-privacy | ✓ | 377us | 5.4 KB |
| 136 | brilliant-numbers |  |  |  |
| 137 | brownian-tree | ✓ | 19.39s | 682.6 KB |
| 138 | bulls-and-cows-player | ✓ | 1.38ms | 1.9 MB |
| 139 | bulls-and-cows |  |  |  |
| 140 | burrows-wheeler-transform | ✓ | 2.67ms | 348.0 KB |
| 141 | caesar-cipher-1 | ✓ | 993us | 248.2 KB |
| 142 | caesar-cipher-2 | ✓ | 1.17ms | 248.2 KB |
| 143 | calculating-the-value-of-e |  |  |  |
| 144 | calendar---for-real-programmers-1 | ✓ | 475us | 34.4 KB |
| 145 | calendar---for-real-programmers-2 | ✓ | 422us | 34.4 KB |
| 146 | calendar | ✓ | 428us | 34.4 KB |
| 147 | calkin-wilf-sequence |  |  |  |
| 148 | call-a-foreign-language-function | ✓ | 325us | 1.6 MB |
| 149 | call-a-function-1 | ✓ | 20us | 0 B |
| 150 | call-a-function-10 | ✓ | 43us | 688 B |
| 151 | call-a-function-11 | ✓ | 248us | 4.9 KB |
| 152 | call-a-function-12 | ✓ | 240us | 4.8 KB |
| 153 | call-a-function-2 | ✓ | 20us | 0 B |
| 154 | call-a-function-3 | ✓ | 100us | 688 B |
| 155 | call-a-function-4 | ✓ | 16us | 0 B |
| 156 | call-a-function-5 | ✓ | 714us | 4.7 KB |
| 157 | call-a-function-6 | ✓ | 274us | 4.9 KB |
| 158 | call-a-function-7 | ✓ | 21us | 0 B |
| 159 | call-a-function-8 | ✓ | 69us | 2.2 KB |
| 160 | call-a-function-9 | ✓ | 76us | 688 B |
| 161 | call-an-object-method-1 | ✓ | 20us | 0 B |
| 162 | call-an-object-method-2 | ✓ | 43us | 0 B |
| 163 | call-an-object-method-3 | ✓ | 23us | 0 B |
| 164 | call-an-object-method | ✓ | 35us | 0 B |
| 165 | camel-case-and-snake-case | ✓ | 594us | 62.7 KB |
| 166 | canny-edge-detector |  |  |  |
| 167 | canonicalize-cidr | ✓ | 679us | 62.4 KB |
| 168 | cantor-set |  |  |  |
| 169 | carmichael-3-strong-pseudoprimes | ✓ | 1.26ms | 34.0 KB |
| 170 | cartesian-product-of-two-or-more-lists-1 | ✓ | 330us | 7.2 KB |
| 171 | cartesian-product-of-two-or-more-lists-2 |  |  |  |
| 172 | cartesian-product-of-two-or-more-lists-3 |  |  |  |
| 173 | cartesian-product-of-two-or-more-lists-4 |  |  |  |
| 174 | case-sensitivity-of-identifiers |  |  |  |
| 175 | casting-out-nines |  |  |  |
| 176 | catalan-numbers-1 |  |  |  |
| 177 | catalan-numbers-2 |  |  |  |
| 178 | catalan-numbers-pascals-triangle |  |  |  |
| 179 | catamorphism |  |  |  |
| 180 | catmull-clark-subdivision-surface |  |  |  |
| 181 | chaocipher |  |  |  |
| 182 | chaos-game |  |  |  |
| 183 | character-codes-1 |  |  |  |
| 184 | character-codes-2 |  |  |  |
| 185 | character-codes-3 |  |  |  |
| 186 | character-codes-4 |  |  |  |
| 187 | character-codes-5 |  |  |  |
| 188 | chat-server |  |  |  |
| 189 | check-machin-like-formulas |  |  |  |
| 190 | check-that-file-exists |  |  |  |
| 191 | checkpoint-synchronization-1 |  |  |  |
| 192 | checkpoint-synchronization-2 |  |  |  |
| 193 | checkpoint-synchronization-3 |  |  |  |
| 194 | checkpoint-synchronization-4 |  |  |  |
| 195 | chernicks-carmichael-numbers |  |  |  |
| 196 | cheryls-birthday |  |  |  |
| 197 | chinese-remainder-theorem |  |  |  |
| 198 | chinese-zodiac |  |  |  |
| 199 | cholesky-decomposition-1 |  |  |  |
| 200 | cholesky-decomposition |  |  |  |
| 201 | chowla-numbers |  |  |  |
| 202 | church-numerals-1 |  |  |  |
| 203 | church-numerals-2 |  |  |  |
| 204 | circles-of-given-radius-through-two-points |  |  |  |
| 205 | circular-primes |  |  |  |
| 206 | cistercian-numerals |  |  |  |
| 207 | comma-quibbling |  |  |  |
| 208 | compiler-virtual-machine-interpreter |  |  |  |
| 209 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k |  |  |  |
| 210 | compound-data-type |  |  |  |
| 211 | concurrent-computing-1 |  |  |  |
| 212 | concurrent-computing-2 |  |  |  |
| 213 | concurrent-computing-3 |  |  |  |
| 214 | conditional-structures-1 |  |  |  |
| 215 | conditional-structures-10 |  |  |  |
| 216 | conditional-structures-2 |  |  |  |
| 217 | conditional-structures-3 |  |  |  |
| 218 | conditional-structures-4 |  |  |  |
| 219 | conditional-structures-5 |  |  |  |
| 220 | conditional-structures-6 |  |  |  |
| 221 | conditional-structures-7 |  |  |  |
| 222 | conditional-structures-8 |  |  |  |
| 223 | conditional-structures-9 |  |  |  |
| 224 | consecutive-primes-with-ascending-or-descending-differences |  |  |  |
| 225 | constrained-genericity-1 |  |  |  |
| 226 | constrained-genericity-2 |  |  |  |
| 227 | constrained-genericity-3 |  |  |  |
| 228 | constrained-genericity-4 |  |  |  |
| 229 | constrained-random-points-on-a-circle-1 |  |  |  |
| 230 | constrained-random-points-on-a-circle-2 |  |  |  |
| 231 | continued-fraction |  |  |  |
| 232 | convert-decimal-number-to-rational |  |  |  |
| 233 | convert-seconds-to-compound-duration |  |  |  |
| 234 | convex-hull |  |  |  |
| 235 | conways-game-of-life |  |  |  |
| 236 | copy-a-string-1 |  |  |  |
| 237 | copy-a-string-2 |  |  |  |
| 238 | copy-stdin-to-stdout-1 |  |  |  |
| 239 | copy-stdin-to-stdout-2 |  |  |  |
| 240 | count-in-factors |  |  |  |
| 241 | count-in-octal-1 |  |  |  |
| 242 | count-in-octal-2 |  |  |  |
| 243 | count-in-octal-3 |  |  |  |
| 244 | count-in-octal-4 |  |  |  |
| 245 | count-occurrences-of-a-substring |  |  |  |
| 246 | count-the-coins-1 |  |  |  |
| 247 | count-the-coins-2 |  |  |  |
| 248 | cramers-rule |  |  |  |
| 249 | crc-32-1 |  |  |  |
| 250 | crc-32-2 |  |  |  |
| 251 | create-a-file-on-magnetic-tape |  |  |  |
| 252 | create-a-file |  |  |  |
| 253 | create-a-two-dimensional-array-at-runtime-1 |  |  |  |
| 254 | create-an-html-table |  |  |  |
| 255 | create-an-object-at-a-given-address |  |  |  |
| 256 | csv-data-manipulation |  |  |  |
| 257 | csv-to-html-translation-1 |  |  |  |
| 258 | csv-to-html-translation-2 |  |  |  |
| 259 | csv-to-html-translation-3 |  |  |  |
| 260 | csv-to-html-translation-4 |  |  |  |
| 261 | csv-to-html-translation-5 |  |  |  |
| 262 | cuban-primes |  |  |  |
| 263 | cullen-and-woodall-numbers |  |  |  |
| 264 | cumulative-standard-deviation |  |  |  |
| 265 | currency |  |  |  |
| 266 | currying |  |  |  |
| 267 | curzon-numbers |  |  |  |
| 268 | cusip |  |  |  |
| 269 | cyclops-numbers |  |  |  |
| 270 | damm-algorithm |  |  |  |
| 271 | date-format |  |  |  |
| 272 | date-manipulation |  |  |  |
| 273 | day-of-the-week |  |  |  |
| 274 | de-bruijn-sequences |  |  |  |
| 275 | deal-cards-for-freecell |  |  |  |
| 276 | death-star |  |  |  |
| 277 | deceptive-numbers |  |  |  |
| 278 | deconvolution-1d-2 |  |  |  |
| 279 | deconvolution-1d-3 |  |  |  |
| 280 | deconvolution-1d |  |  |  |
| 281 | deepcopy-1 |  |  |  |
| 282 | define-a-primitive-data-type |  |  |  |
| 283 | delegates |  |  |  |
| 284 | demings-funnel |  |  |  |
| 285 | department-numbers |  |  |  |
| 286 | descending-primes |  |  |  |
| 287 | detect-division-by-zero |  |  |  |
| 288 | determine-if-a-string-has-all-the-same-characters |  |  |  |
| 289 | determine-if-a-string-has-all-unique-characters |  |  |  |
| 290 | determine-if-a-string-is-collapsible |  |  |  |
| 291 | determine-if-a-string-is-numeric-1 |  |  |  |
| 292 | determine-if-a-string-is-numeric-2 |  |  |  |
| 293 | determine-if-a-string-is-squeezable |  |  |  |
| 294 | determine-if-only-one-instance-is-running |  |  |  |
| 295 | determine-if-two-triangles-overlap |  |  |  |
| 296 | determine-sentence-type |  |  |  |
| 297 | dice-game-probabilities-1 |  |  |  |
| 298 | dice-game-probabilities-2 |  |  |  |
| 299 | digital-root-multiplicative-digital-root |  |  |  |
| 300 | dijkstras-algorithm |  |  |  |
| 301 | dinesmans-multiple-dwelling-problem |  |  |  |
| 302 | dining-philosophers-1 |  |  |  |
| 303 | dining-philosophers-2 |  |  |  |
| 304 | disarium-numbers |  |  |  |
| 305 | discordian-date |  |  |  |
| 306 | display-a-linear-combination |  |  |  |
| 307 | display-an-outline-as-a-nested-table |  |  |  |
| 308 | distance-and-bearing |  |  |  |
| 309 | distributed-programming |  |  |  |
| 310 | diversity-prediction-theorem |  |  |  |
| 311 | dns-query |  |  |  |
| 312 | documentation |  |  |  |
| 313 | doomsday-rule |  |  |  |
| 314 | dot-product |  |  |  |
| 315 | doubly-linked-list-definition-1 |  |  |  |
| 316 | doubly-linked-list-definition-2 |  |  |  |
| 317 | doubly-linked-list-element-definition |  |  |  |
| 318 | doubly-linked-list-traversal |  |  |  |
| 319 | dragon-curve |  |  |  |
| 320 | draw-a-clock |  |  |  |
| 321 | draw-a-cuboid |  |  |  |
| 322 | draw-a-pixel-1 |  |  |  |
| 323 | draw-a-rotating-cube |  |  |  |
| 324 | draw-a-sphere |  |  |  |
| 325 | duffinian-numbers |  |  |  |
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
| 377 | execute-computer-zero-1 |  |  |  |
| 378 | execute-computer-zero |  |  |  |
| 379 | execute-hq9+ |  |  |  |
| 380 | execute-snusp |  |  |  |
| 381 | exponentiation-operator-2 |  |  |  |
| 382 | exponentiation-operator |  |  |  |
| 383 | exponentiation-order |  |  |  |
| 384 | exponentiation-with-infix-operators-in-or-operating-on-the-base |  |  |  |
| 385 | extend-your-language |  |  |  |
| 386 | extensible-prime-generator |  |  |  |
| 387 | extreme-floating-point-values |  |  |  |
| 388 | faces-from-a-mesh-2 |  |  |  |
| 389 | faces-from-a-mesh |  |  |  |
| 390 | factorial-base-numbers-indexing-permutations-of-a-collection |  |  |  |
| 391 | factorial-primes |  |  |  |
| 392 | factorial |  |  |  |
| 393 | factorions |  |  |  |
| 394 | factors-of-a-mersenne-number |  |  |  |
| 395 | factors-of-an-integer |  |  |  |
| 396 | fairshare-between-two-and-more |  |  |  |
| 397 | farey-sequence |  |  |  |
| 398 | fast-fourier-transform |  |  |  |
| 399 | fasta-format |  |  |  |
| 400 | faulhabers-formula |  |  |  |
| 401 | faulhabers-triangle |  |  |  |
| 402 | feigenbaum-constant-calculation |  |  |  |
| 403 | fermat-numbers |  |  |  |
| 404 | fibonacci-n-step-number-sequences |  |  |  |
| 405 | fibonacci-sequence-1 |  |  |  |
| 406 | fibonacci-sequence-2 |  |  |  |
| 407 | fibonacci-sequence-3 |  |  |  |
| 408 | fibonacci-sequence-4 |  |  |  |
| 409 | fibonacci-sequence-5 |  |  |  |
| 410 | fibonacci-word-fractal |  |  |  |
| 411 | fibonacci-word |  |  |  |
| 412 | file-extension-is-in-extensions-list |  |  |  |
| 413 | file-input-output-1 |  |  |  |
| 414 | file-input-output-2 |  |  |  |
| 415 | file-input-output-3 |  |  |  |
| 416 | file-modification-time |  |  |  |
| 417 | file-size-distribution |  |  |  |
| 418 | file-size |  |  |  |
| 419 | filter |  |  |  |
| 420 | find-chess960-starting-position-identifier-2 |  |  |  |
| 421 | find-chess960-starting-position-identifier |  |  |  |
| 422 | find-common-directory-path |  |  |  |
| 423 | find-duplicate-files |  |  |  |
| 424 | find-largest-left-truncatable-prime-in-a-given-base |  |  |  |
| 425 | find-limit-of-recursion |  |  |  |
| 426 | find-palindromic-numbers-in-both-binary-and-ternary-bases |  |  |  |
| 427 | find-the-intersection-of-a-line-with-a-plane | ✓ | 485us | 5.1 KB |
| 428 | find-the-intersection-of-two-lines |  |  |  |
| 429 | find-the-last-sunday-of-each-month |  |  |  |
| 430 | find-the-missing-permutation | ✓ | 223us | 7.8 KB |
| 431 | first-class-environments |  |  |  |
| 432 | first-class-functions-use-numbers-analogously |  |  |  |
| 433 | first-power-of-2-that-has-leading-decimal-digits-of-12 |  |  |  |
| 434 | five-weekends |  |  |  |
| 435 | fivenum-1 |  |  |  |
| 436 | fivenum-2 |  |  |  |
| 437 | fivenum-3 |  |  |  |
| 438 | fixed-length-records-1 |  |  |  |
| 439 | fixed-length-records-2 |  |  |  |
| 440 | fizzbuzz-1 |  |  |  |
| 441 | fizzbuzz-2 |  |  |  |
| 442 | fizzbuzz |  |  |  |
| 443 | flatten-a-list-1 |  |  |  |
| 444 | flatten-a-list-2 |  |  |  |
| 445 | flipping-bits-game |  |  |  |
| 446 | flow-control-structures-1 |  |  |  |
| 447 | flow-control-structures-2 |  |  |  |
| 448 | flow-control-structures-3 |  |  |  |
| 449 | flow-control-structures-4 |  |  |  |
| 450 | floyd-warshall-algorithm |  |  |  |
| 451 | floyd-warshall-algorithm2 | ✓ | 323us | 12.1 KB |
| 452 | floyds-triangle |  |  |  |
| 453 | forest-fire |  |  |  |
| 454 | fork-2 |  |  |  |
| 455 | fork |  |  |  |
| 456 | formal-power-series |  |  |  |
| 457 | formatted-numeric-output |  |  |  |
| 458 | forward-difference |  |  |  |
| 459 | four-bit-adder-1 |  |  |  |
| 460 | four-is-magic |  |  |  |
| 461 | four-is-the-number-of-letters-in-the-... |  |  |  |
| 462 | fractal-tree |  |  |  |
| 463 | fractran |  |  |  |
| 464 | french-republican-calendar |  |  |  |
| 465 | ftp |  |  |  |
| 466 | function-frequency |  |  |  |
| 467 | function-prototype |  |  |  |
| 468 | functional-coverage-tree |  |  |  |
| 469 | fusc-sequence |  |  |  |
| 470 | gamma-function |  |  |  |
| 471 | general-fizzbuzz |  |  |  |
| 472 | generic-swap |  |  |  |
| 473 | get-system-command-output |  |  |  |
| 474 | giuga-numbers |  |  |  |
| 475 | globally-replace-text-in-several-files |  |  |  |
| 476 | goldbachs-comet |  |  |  |
| 477 | golden-ratio-convergence | ✓ | 1.18ms | 4.9 KB |
| 478 | graph-colouring | ✓ | 372us | 4.7 KB |
| 479 | gray-code | ✓ | 947us | 28.6 KB |
| 480 | gui-component-interaction | ✓ | 690us | 9.0 KB |
| 481 | gui-enabling-disabling-of-controls | ✓ | 704us | 12.5 KB |
| 482 | gui-maximum-window-dimensions | ✓ | 698us | 5.0 KB |
| 483 | http |  |  |  |
| 484 | image-noise | ✓ | 309.55ms | 75.0 MB |
| 485 | loops-increment-loop-index-within-loop-body |  |  |  |
| 486 | md5 | ✓ | 60.63ms | 2.0 KB |
| 487 | nim-game | ✓ | 721us | 9.8 KB |
| 488 | plasma-effect | ✓ | 5.17ms | 4.7 KB |
| 489 | sorting-algorithms-bubble-sort | ✓ | 484us | 5.4 KB |
| 490 | window-management | ✓ | 353us | 12.1 KB |
| 491 | zumkeller-numbers | ✓ | 1.58s | 24.2 MB |
