# C++ Transpiler Rosetta Output

This directory stores C++ code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.cpp` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (268/491) - Last updated 2025-08-03 14:32 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 1 | 100-doors-2 | ✓ | 525.0µs | 256.00KB |
| 2 | 100-doors-3 | ✓ | 206.0µs | 12.62MB |
| 3 | 100-doors | ✓ | 485.0µs | 12.42MB |
| 4 | 100-prisoners | ✓ | 915.0ms | 12.78MB |
| 5 | 15-puzzle-game | ✓ | 258.0µs | 14.07MB |
| 6 | 15-puzzle-solver | ✓ | 141.0µs | 12.91MB |
| 7 | 2048 | ✓ | 689.0µs | 13.73MB |
| 8 | 21-game | ✓ | 356.0µs | 13.36MB |
| 9 | 24-game-solve | ✓ | 4.0ms | 13.75MB |
| 10 | 24-game | ✓ | 298.0µs | 3.83MB |
| 11 | 4-rings-or-4-squares-puzzle | ✓ | 232.0µs | 4.36MB |
| 12 | 9-billion-names-of-god-the-integer | ✓ | 126.70s | 219.41MB |
| 13 | 99-bottles-of-beer-2 | ✓ | 35.0ms | 14.44MB |
| 14 | 99-bottles-of-beer | ✓ | 390.0µs | 13.00MB |
| 15 | a+b | ✓ | 46.0µs | 12.06MB |
| 16 | abbreviations-automatic | ✓ | 11.0ms | 14.06MB |
| 17 | abbreviations-easy | ✓ | 1.0ms | 13.74MB |
| 18 | abbreviations-simple | ✓ | 3.0ms | 14.25MB |
| 19 | abc-problem | ✓ | 2.0ms | 3.52MB |
| 20 | abelian-sandpile-model-identity | ✓ |  |  |
| 21 | abelian-sandpile-model | ✓ | 612.0µs | 128.00KB |
| 22 | abstract-type | ✓ | 113.0µs | 13.05MB |
| 23 | abundant-deficient-and-perfect-number-classifications | ✓ | 222.0ms | 12.93MB |
| 24 | abundant-odd-numbers | ✓ | 1.10s | 12.94MB |
| 25 | accumulator-factory | ✓ | 71.0µs | 12.79MB |
| 26 | achilles-numbers | ✓ | 21.0ms | 14.59MB |
| 27 | ackermann-function-2 | ✓ | 52.0µs | 13.44MB |
| 28 | ackermann-function-3 | ✓ |  |  |
| 29 | ackermann-function | ✓ | 217.0µs | 12.62MB |
| 30 | active-directory-connect | ✓ | 44.0µs | 12.87MB |
| 31 | active-directory-search-for-a-user | ✓ | 66.0µs | 12.94MB |
| 32 | active-object | ✓ | 85.0µs | 13.00MB |
| 33 | add-a-variable-to-a-class-instance-at-runtime | ✓ | 262.0µs | 3.77MB |
| 34 | additive-primes | ✓ | 162.0µs | 13.29MB |
| 35 | address-of-a-variable | ✓ | 83.0µs | 12.34MB |
| 36 | adfgvx-cipher |   |  |  |
| 37 | aks-test-for-primes | ✓ | 114.0µs | 13.14MB |
| 38 | algebraic-data-types |   |  |  |
| 39 | align-columns | ✓ | 473.0µs | 13.67MB |
| 40 | aliquot-sequence-classifications | ✓ | 442.0µs | 13.93MB |
| 41 | almkvist-giullera-formula-for-pi |   |  |  |
| 42 | almost-prime | ✓ | 144.0µs | 12.99MB |
| 43 | amb | ✓ | 70.0µs | 13.10MB |
| 44 | amicable-pairs | ✓ | 326.0ms | 13.35MB |
| 45 | anagrams-deranged-anagrams | ✓ | 228.0µs | 13.68MB |
| 46 | anagrams | ✓ | 640.0µs | 13.48MB |
| 47 | angle-difference-between-two-bearings-1 | ✓ | 96.0µs | 12.93MB |
| 48 | angle-difference-between-two-bearings-2 | ✓ | 80.0µs | 12.96MB |
| 49 | angles-geometric-normalization-and-conversion | ✓ | 380.0µs | 13.27MB |
| 50 | animate-a-pendulum | ✓ | 320.0µs | 13.16MB |
| 51 | animation | ✓ | 491.0µs | 12.45MB |
| 52 | anonymous-recursion-1 | ✓ | 73.0µs | 12.67MB |
| 53 | anonymous-recursion-2 | ✓ | 68.0µs | 13.05MB |
| 54 | anonymous-recursion | ✓ | 72.0µs | 12.93MB |
| 55 | anti-primes | ✓ | 28.0ms | 12.88MB |
| 56 | append-a-record-to-the-end-of-a-text-file | ✓ | 36.0µs | 12.62MB |
| 57 | apply-a-callback-to-an-array-1 | ✓ | 57.0µs | 12.69MB |
| 58 | apply-a-callback-to-an-array-2 | ✓ | 74.0µs | 13.40MB |
| 59 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 3.0ms | 13.32MB |
| 60 | approximate-equality | ✓ | 186.0µs | 13.23MB |
| 61 | arbitrary-precision-integers-included- | ✓ | 1.0ms | 13.24MB |
| 62 | archimedean-spiral | ✓ | 9.0ms | 13.12MB |
| 63 | arena-storage-pool | ✓ |  |  |
| 64 | arithmetic-complex | ✓ | 424.0µs | 12.80MB |
| 65 | arithmetic-derivative | ✓ | 2.0ms | 13.12MB |
| 66 | arithmetic-evaluation | ✓ | 346.0µs | 13.70MB |
| 67 | arithmetic-geometric-mean-calculate-pi | ✓ | 463.0µs | 12.61MB |
| 68 | arithmetic-geometric-mean | ✓ | 440.0µs | 13.05MB |
| 69 | arithmetic-integer-1 | ✓ | 256.0µs | 12.80MB |
| 70 | arithmetic-integer-2 | ✓ | 197.0µs | 13.10MB |
| 71 | arithmetic-numbers |   |  |  |
| 72 | arithmetic-rational | ✓ | 1.0ms | 12.86MB |
| 73 | array-concatenation | ✓ | 348.0µs | 13.04MB |
| 74 | array-length | ✓ | 255.0µs | 12.74MB |
| 75 | arrays | ✓ | 394.0µs | 13.24MB |
| 76 | ascending-primes | ✓ | 917.0µs | 13.05MB |
| 77 | ascii-art-diagram-converter | ✓ | 304.0µs | 12.62MB |
| 78 | assertions | ✓ | 234.0µs | 12.75MB |
| 79 | associative-array-creation |   |  |  |
| 80 | associative-array-iteration | ✓ | 336.0µs | 13.03MB |
| 81 | associative-array-merging |   |  |  |
| 82 | atomic-updates |   |  |  |
| 83 | attractive-numbers | ✓ | 420.0µs | 12.94MB |
| 84 | average-loop-length |   |  |  |
| 85 | averages-arithmetic-mean |   |  |  |
| 86 | averages-mean-time-of-day | ✓ | 284.0µs | 13.93MB |
| 87 | averages-median-1 | ✓ | 413.0µs | 12.78MB |
| 88 | averages-median-2 | ✓ | 380.0µs | 12.90MB |
| 89 | averages-median-3 | ✓ | 398.0µs | 13.63MB |
| 90 | averages-mode | ✓ | 246.0µs | 13.37MB |
| 91 | averages-pythagorean-means | ✓ | 421.0µs | 13.16MB |
| 92 | averages-root-mean-square | ✓ | 326.0µs | 12.67MB |
| 93 | averages-simple-moving-average | ✓ | 517.0µs | 12.86MB |
| 94 | avl-tree |   |  |  |
| 95 | b-zier-curves-intersections | ✓ | 20.0ms | 14.11MB |
| 96 | babbage-problem | ✓ | 374.0µs | 12.66MB |
| 97 | babylonian-spiral | ✓ | 1.0ms | 14.18MB |
| 98 | balanced-brackets |   | 354.0µs | 13.25MB |
| 99 | balanced-ternary | ✓ | 118.0µs | 14.67MB |
| 100 | barnsley-fern | ✓ | 96.0ms | 13.11MB |
| 101 | base64-decode-data | ✓ | 55.0µs | 14.15MB |
| 102 | bell-numbers | ✓ | 4.0ms | 13.18MB |
| 103 | benfords-law | ✓ | 1.0ms | 13.39MB |
| 104 | bernoulli-numbers |   | 239.0ms | 13.11MB |
| 105 | best-shuffle | ✓ | 360.0µs | 13.79MB |
| 106 | bifid-cipher | ✓ | 2.0ms | 14.81MB |
| 107 | bin-given-limits | ✓ | 331.0µs | 14.11MB |
| 108 | binary-digits | ✓ | 423.0µs | 12.69MB |
| 109 | binary-search | ✓ | 200.0µs | 13.05MB |
| 110 | binary-strings | ✓ | 227.0µs | 13.02MB |
| 111 | bioinformatics-base-count | ✓ | 341.0µs | 13.24MB |
| 112 | bioinformatics-global-alignment | ✓ | 20.62s | 14.24MB |
| 113 | bioinformatics-sequence-mutation | ✓ | 59.0ms | 14.20MB |
| 114 | biorhythms | ✓ | 802.0µs | 14.48MB |
| 115 | bitcoin-address-validation | ✓ | 1.0ms | 13.63MB |
| 116 | bitmap-b-zier-curves-cubic | ✓ | 183.0ms | 14.18MB |
| 117 | bitmap-b-zier-curves-quadratic | ✓ | 187.0ms | 14.15MB |
| 118 | bitmap-bresenhams-line-algorithm | ✓ | 281.0µs | 13.12MB |
| 119 | bitmap-flood-fill | ✓ | 192.0µs | 12.69MB |
| 120 | bitmap-histogram | ✓ | 479.0µs | 256.00KB |
| 121 | bitmap-midpoint-circle-algorithm | ✓ | 374.0µs | 12.62MB |
| 122 | bitmap-ppm-conversion-through-a-pipe |   |  |  |
| 123 | bitmap-read-a-ppm-file | ✓ | 163.0µs | 14.37MB |
| 124 | bitmap-read-an-image-through-a-pipe | ✓ | 47.0µs | 12.98MB |
| 125 | bitmap-write-a-ppm-file | ✓ | 119.0µs | 13.36MB |
| 126 | bitmap |   |  |  |
| 127 | bitwise-io-1 |   | 246.0µs | 13.08MB |
| 128 | bitwise-io-2 | ✓ | 574.0µs | 14.68MB |
| 129 | bitwise-operations | ✓ | 233.0µs | 13.95MB |
| 130 | blum-integer | ✓ | 200.0µs | 14.06MB |
| 131 | boolean-values | ✓ | 48.0µs | 12.94MB |
| 132 | box-the-compass | ✓ | 488.0µs | 13.20MB |
| 133 | boyer-moore-string-search | ✓ | 280.0µs | 13.25MB |
| 134 | brazilian-numbers | ✓ | 8.61s | 13.38MB |
| 135 | break-oo-privacy | ✓ | 61.0µs | 13.20MB |
| 136 | brilliant-numbers |   |  |  |
| 137 | brownian-tree | ✓ | 174.41s | 13.12MB |
| 138 | bulls-and-cows-player | ✓ | 843.0ms | 13.92MB |
| 139 | bulls-and-cows | ✓ | 159.0µs | 13.12MB |
| 140 | burrows-wheeler-transform | ✓ | 15.0ms | 13.93MB |
| 141 | caesar-cipher-1 | ✓ | 353.0µs | 12.75MB |
| 142 | caesar-cipher-2 | ✓ | 377.0µs | 13.46MB |
| 143 | calculating-the-value-of-e | ✓ | 356.0µs | 13.03MB |
| 144 | calendar---for-real-programmers-1 | ✓ | 829.0µs | 13.11MB |
| 145 | calendar---for-real-programmers-2 | ✓ | 655.0µs | 13.56MB |
| 146 | calendar | ✓ | 704.0µs | 13.25MB |
| 147 | calkin-wilf-sequence |   | 415.0µs | 13.49MB |
| 148 | call-a-foreign-language-function | ✓ | 152.0µs | 12.61MB |
| 149 | call-a-function-1 | ✓ | 10.0µs | 12.54MB |
| 150 | call-a-function-10 | ✓ | 36.0µs | 12.54MB |
| 151 | call-a-function-11 | ✓ | 311.0µs | 12.50MB |
| 152 | call-a-function-12 | ✓ | 250.0µs | 12.89MB |
| 153 | call-a-function-2 | ✓ | 23.0µs | 12.93MB |
| 154 | call-a-function-3 | ✓ | 50.0µs | 12.73MB |
| 155 | call-a-function-4 | ✓ | 48.0µs | 12.68MB |
| 156 | call-a-function-5 | ✓ | 252.0µs | 12.96MB |
| 157 | call-a-function-6 | ✓ | 253.0µs | 12.57MB |
| 158 | call-a-function-7 | ✓ | 22.0µs | 12.79MB |
| 159 | call-a-function-8 | ✓ | 57.0µs | 12.88MB |
| 160 | call-a-function-9 | ✓ | 56.0µs | 12.79MB |
| 161 | call-an-object-method-1 | ✓ | 15.0µs | 12.30MB |
| 162 | call-an-object-method-2 |   |  |  |
| 163 | call-an-object-method-3 | ✓ | 3.0µs | 12.62MB |
| 164 | call-an-object-method | ✓ | 2.0µs | 12.69MB |
| 165 | camel-case-and-snake-case | ✓ | 1.0ms | 14.19MB |
| 166 | canny-edge-detector | ✓ | 212.0µs | 14.18MB |
| 167 | canonicalize-cidr | ✓ | 666.0µs | 13.99MB |
| 168 | cantor-set | ✓ | 681.0µs | 13.09MB |
| 169 | carmichael-3-strong-pseudoprimes | ✓ | 737.0µs | 13.35MB |
| 170 | cartesian-product-of-two-or-more-lists-1 | ✓ | 284.0µs | 13.12MB |
| 171 | cartesian-product-of-two-or-more-lists-2 | ✓ | 71.0µs | 14.17MB |
| 172 | cartesian-product-of-two-or-more-lists-3 | ✓ | 296.0µs | 13.56MB |
| 173 | cartesian-product-of-two-or-more-lists-4 | ✓ | 55.0µs | 13.16MB |
| 174 | case-sensitivity-of-identifiers | ✓ | 157.0µs | 13.62MB |
| 175 | casting-out-nines | ✓ | 4.0ms | 13.41MB |
| 176 | catalan-numbers-1 | ✓ | 59.0µs | 12.89MB |
| 177 | catalan-numbers-2 | ✓ | 51.0µs | 13.16MB |
| 178 | catalan-numbers-pascals-triangle | ✓ | 73.0µs | 12.96MB |
| 179 | catamorphism | ✓ | 22.0µs | 12.98MB |
| 180 | catmull-clark-subdivision-surface | ✓ | 1.0ms | 14.62MB |
| 181 | chaocipher | ✓ | 403.0µs | 13.44MB |
| 182 | chaos-game | ✓ | 28.0ms | 12.88MB |
| 183 | character-codes-1 | ✓ | 42.0µs | 12.60MB |
| 184 | character-codes-2 | ✓ | 42.0µs | 13.25MB |
| 185 | character-codes-3 | ✓ | 58.0µs | 13.36MB |
| 186 | character-codes-4 | ✓ | 26.0µs | 12.68MB |
| 187 | character-codes-5 | ✓ | 28.0µs | 12.80MB |
| 188 | chat-server | ✓ | 53.0µs | 13.29MB |
| 189 | check-machin-like-formulas | ✓ | 54.0ms | 14.19MB |
| 190 | check-that-file-exists | ✓ | 297.0µs | 13.13MB |
| 191 | checkpoint-synchronization-1 | ✓ | 188.0µs | 13.18MB |
| 192 | checkpoint-synchronization-2 | ✓ | 261.0µs | 13.05MB |
| 193 | checkpoint-synchronization-3 | ✓ | 290.0µs | 13.00MB |
| 194 | checkpoint-synchronization-4 | ✓ | 356.0µs | 12.62MB |
| 195 | chernicks-carmichael-numbers | ✓ | 334.0ms | 13.98MB |
| 196 | cheryls-birthday | ✓ | 221.0µs | 13.49MB |
| 197 | chinese-remainder-theorem | ✓ | 175.0µs | 12.80MB |
| 198 | chinese-zodiac | ✓ | 285.0µs | 13.36MB |
| 199 | cholesky-decomposition-1 |   |  |  |
| 200 | cholesky-decomposition | ✓ | 573.0µs | 13.04MB |
| 201 | chowla-numbers | ✓ | 174.0µs | 13.23MB |
| 202 | church-numerals-1 | ✓ | 677.0µs | 13.62MB |
| 203 | church-numerals-2 |   |  |  |
| 204 | circles-of-given-radius-through-two-points | ✓ | 347.0µs | 14.15MB |
| 205 | circular-primes | ✓ | 814.0µs | 13.68MB |
| 206 | cistercian-numerals | ✓ | 12.0ms | 14.62MB |
| 207 | comma-quibbling | ✓ | 172.0µs | 12.96MB |
| 208 | compiler-virtual-machine-interpreter |   |  |  |
| 209 | composite-numbers-k-with-no-single-digit-factors-whose-factors-are-all-substrings-of-k | ✓ | 27.29s | 14.12MB |
| 210 | compound-data-type | ✓ | 4.0µs | 11.86MB |
| 211 | concurrent-computing-1 | ✓ | 196.0µs | 12.61MB |
| 212 | concurrent-computing-2 | ✓ | 195.0µs | 12.71MB |
| 213 | concurrent-computing-3 | ✓ | 207.0µs | 12.87MB |
| 214 | conditional-structures-1 | ✓ | 22.0µs | 12.65MB |
| 215 | conditional-structures-10 | ✓ | 296.0µs | 12.88MB |
| 216 | conditional-structures-2 | ✓ | 23.0µs | 12.71MB |
| 217 | conditional-structures-3 | ✓ | 16.0µs | 12.83MB |
| 218 | conditional-structures-4 | ✓ | 5.0µs | 13.15MB |
| 219 | conditional-structures-5 | ✓ | 4.0µs | 12.74MB |
| 220 | conditional-structures-6 | ✓ | 17.0µs | 13.06MB |
| 221 | conditional-structures-7 | ✓ | 4.0µs | 12.61MB |
| 222 | conditional-structures-8 | ✓ | 4.0µs | 12.70MB |
| 223 | conditional-structures-9 | ✓ | 4.0µs | 12.79MB |
| 224 | consecutive-primes-with-ascending-or-descending-differences | ✓ | 87.0ms | 13.77MB |
| 225 | constrained-genericity-1 | ✓ | 4.0µs | 12.74MB |
| 226 | constrained-genericity-2 | ✓ | 4.0µs | 12.41MB |
| 227 | constrained-genericity-3 | ✓ | 5.0µs | 12.73MB |
| 228 | constrained-genericity-4 | ✓ | 150.0µs | 12.96MB |
| 229 | constrained-random-points-on-a-circle-1 |   |  |  |
| 230 | constrained-random-points-on-a-circle-2 | ✓ | 46.0ms | 412.00KB |
| 231 | continued-fraction |   |  |  |
| 232 | convert-decimal-number-to-rational |   |  |  |
| 233 | convert-seconds-to-compound-duration |   |  |  |
| 234 | convex-hull |   |  |  |
| 235 | conways-game-of-life |   |  |  |
| 236 | copy-a-string-1 |   |  |  |
| 237 | copy-a-string-2 |   |  |  |
| 238 | copy-stdin-to-stdout-1 |   |  |  |
| 239 | copy-stdin-to-stdout-2 |   |  |  |
| 240 | count-in-factors |   |  |  |
| 241 | count-in-octal-1 |   |  |  |
| 242 | count-in-octal-2 |   |  |  |
| 243 | count-in-octal-3 | ✓ | 247.0µs | 12.94MB |
| 244 | count-in-octal-4 | ✓ | 149.0µs | 12.80MB |
| 245 | count-occurrences-of-a-substring | ✓ | 285.0µs | 12.79MB |
| 246 | count-the-coins-1 | ✓ | 214.0µs | 12.93MB |
| 247 | count-the-coins-2 | ✓ | 955.0µs | 12.93MB |
| 248 | cramers-rule | ✓ | 611.0µs | 12.88MB |
| 249 | crc-32-1 | ✓ | 167.0µs | 13.25MB |
| 250 | crc-32-2 | ✓ | 198.0µs | 12.80MB |
| 251 | create-a-file-on-magnetic-tape | ✓ | 58.0µs | 12.61MB |
| 252 | create-a-file | ✓ | 198.0µs | 12.68MB |
| 253 | create-a-two-dimensional-array-at-runtime-1 | ✓ | 247.0µs | 13.29MB |
| 254 | create-an-html-table | ✓ | 215.0µs | 12.80MB |
| 255 | create-an-object-at-a-given-address | ✓ | 229.0µs | 12.89MB |
| 256 | csv-data-manipulation | ✓ | 221.0µs | 13.04MB |
| 257 | csv-to-html-translation-1 | ✓ | 182.0µs | 12.99MB |
| 258 | csv-to-html-translation-2 | ✓ | 202.0µs | 12.86MB |
| 259 | csv-to-html-translation-3 | ✓ | 145.0µs | 12.85MB |
| 260 | csv-to-html-translation-4 | ✓ | 141.0µs | 12.99MB |
| 261 | csv-to-html-translation-5 | ✓ | 429.0µs | 13.38MB |
| 262 | cuban-primes |   |  |  |
| 263 | cullen-and-woodall-numbers |   |  |  |
| 264 | cumulative-standard-deviation | ✓ |  |  |
| 265 | currency | ✓ |  |  |
| 266 | currying |   |  |  |
| 267 | curzon-numbers | ✓ |  |  |
| 268 | cusip | ✓ |  |  |
| 269 | cyclops-numbers |   |  |  |
| 270 | damm-algorithm | ✓ |  |  |
| 271 | date-format | ✓ |  |  |
| 272 | date-manipulation |   |  |  |
| 273 | day-of-the-week | ✓ |  |  |
| 274 | de-bruijn-sequences |   |  |  |
| 275 | deal-cards-for-freecell |   |  |  |
| 276 | death-star |   |  |  |
| 277 | deceptive-numbers |   |  |  |
| 278 | deconvolution-1d-2 | ✓ |  |  |
| 279 | deconvolution-1d-3 | ✓ |  |  |
| 280 | deconvolution-1d | ✓ |  |  |
| 281 | deepcopy-1 |   |  |  |
| 282 | define-a-primitive-data-type |   |  |  |
| 283 | delegates |   |  |  |
| 284 | demings-funnel |   |  |  |
| 285 | department-numbers | ✓ |  |  |
| 286 | descending-primes |   |  |  |
| 287 | detect-division-by-zero | ✓ |  |  |
| 288 | determine-if-a-string-has-all-the-same-characters | ✓ |  |  |
| 289 | determine-if-a-string-has-all-unique-characters | ✓ |  |  |
| 290 | determine-if-a-string-is-collapsible | ✓ |  |  |
| 291 | determine-if-a-string-is-numeric-1 | ✓ |  |  |
| 292 | determine-if-a-string-is-numeric-2 |   |  |  |
| 293 | determine-if-a-string-is-squeezable | ✓ |  |  |
| 294 | determine-if-only-one-instance-is-running | ✓ |  |  |
| 295 | determine-if-two-triangles-overlap | ✓ |  |  |
| 296 | determine-sentence-type | ✓ |  |  |
| 297 | dice-game-probabilities-1 | ✓ |  |  |
| 298 | dice-game-probabilities-2 |   |  |  |
| 299 | digital-root-multiplicative-digital-root |   |  |  |
| 300 | dijkstras-algorithm |   |  |  |
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
| 399 | fasta-format | ✓ |  |  |
| 400 | faulhabers-formula | ✓ |  |  |
| 401 | faulhabers-triangle | ✓ |  |  |
| 402 | feigenbaum-constant-calculation | ✓ |  |  |
| 403 | fermat-numbers | ✓ |  |  |
| 404 | fibonacci-n-step-number-sequences | ✓ |  |  |
| 405 | fibonacci-sequence-1 | ✓ |  |  |
| 406 | fibonacci-sequence-2 | ✓ |  |  |
| 407 | fibonacci-sequence-3 | ✓ |  |  |
| 408 | fibonacci-sequence-4 | ✓ |  |  |
| 409 | fibonacci-sequence-5 | ✓ |  |  |
| 410 | fibonacci-word-fractal | ✓ |  |  |
| 411 | fibonacci-word | ✓ |  |  |
| 412 | file-extension-is-in-extensions-list | ✓ |  |  |
| 413 | file-input-output-1 |   |  |  |
| 414 | file-input-output-2 |   |  |  |
| 415 | file-input-output-3 |   |  |  |
| 416 | file-modification-time |   |  |  |
| 417 | file-size-distribution |   |  |  |
| 418 | file-size |   |  |  |
| 419 | filter |   |  |  |
| 420 | find-chess960-starting-position-identifier-2 |   |  |  |
| 421 | find-chess960-starting-position-identifier |   |  |  |
| 422 | find-common-directory-path |   |  |  |
| 423 | find-duplicate-files |   |  |  |
| 424 | find-largest-left-truncatable-prime-in-a-given-base |   |  |  |
| 425 | find-limit-of-recursion |   |  |  |
| 426 | find-palindromic-numbers-in-both-binary-and-ternary-bases |   |  |  |
| 427 | find-the-intersection-of-a-line-with-a-plane |   |  |  |
| 428 | find-the-intersection-of-two-lines |   |  |  |
| 429 | find-the-last-sunday-of-each-month |   |  |  |
| 430 | find-the-missing-permutation |   |  |  |
| 431 | first-class-environments |   |  |  |
| 432 | first-class-functions-use-numbers-analogously |   |  |  |
| 433 | first-power-of-2-that-has-leading-decimal-digits-of-12 |   |  |  |
| 434 | five-weekends |   |  |  |
| 435 | fivenum-1 |   |  |  |
| 436 | fivenum-2 |   |  |  |
| 437 | fivenum-3 |   |  |  |
| 438 | fixed-length-records-1 |   |  |  |
| 439 | fixed-length-records-2 |   |  |  |
| 440 | fizzbuzz-1 |   |  |  |
| 441 | fizzbuzz-2 |   |  |  |
| 442 | fizzbuzz |   |  |  |
| 443 | flatten-a-list-1 |   |  |  |
| 444 | flatten-a-list-2 |   |  |  |
| 445 | flipping-bits-game |   |  |  |
| 446 | flow-control-structures-1 |   |  |  |
| 447 | flow-control-structures-2 |   |  |  |
| 448 | flow-control-structures-3 |   |  |  |
| 449 | flow-control-structures-4 |   |  |  |
| 450 | floyd-warshall-algorithm | ✓ |  |  |
| 451 | floyd-warshall-algorithm2 | ✓ |  |  |
| 452 | floyds-triangle | ✓ |  |  |
| 453 | forest-fire | ✓ |  |  |
| 454 | fork-2 | ✓ |  |  |
| 455 | fork | ✓ |  |  |
| 456 | formal-power-series |   |  |  |
| 457 | formatted-numeric-output |   |  |  |
| 458 | forward-difference |   |  |  |
| 459 | four-bit-adder-1 |   |  |  |
| 460 | four-is-magic |   |  |  |
| 461 | four-is-the-number-of-letters-in-the-... |   |  |  |
| 462 | fractal-tree |   |  |  |
| 463 | fractran |   |  |  |
| 464 | french-republican-calendar |   |  |  |
| 465 | ftp |   |  |  |
| 466 | function-frequency |   |  |  |
| 467 | function-prototype |   |  |  |
| 468 | functional-coverage-tree |   |  |  |
| 469 | fusc-sequence |   |  |  |
| 470 | gamma-function |   |  |  |
| 471 | general-fizzbuzz |   |  |  |
| 472 | generic-swap |   |  |  |
| 473 | get-system-command-output |   |  |  |
| 474 | giuga-numbers |   |  |  |
| 475 | globally-replace-text-in-several-files |   |  |  |
| 476 | goldbachs-comet |   |  |  |
| 477 | golden-ratio-convergence |   |  |  |
| 478 | graph-colouring |   |  |  |
| 479 | gray-code |   |  |  |
| 480 | gui-component-interaction |   |  |  |
| 481 | gui-enabling-disabling-of-controls |   |  |  |
| 482 | gui-maximum-window-dimensions |   |  |  |
| 483 | http |   |  |  |
| 484 | image-noise |   |  |  |
| 485 | loops-increment-loop-index-within-loop-body |   |  |  |
| 486 | md5 |   |  |  |
| 487 | nim-game |   |  |  |
| 488 | plasma-effect |   |  |  |
| 489 | sorting-algorithms-bubble-sort |   |  |  |
| 490 | window-management |   |  |  |
| 491 | zumkeller-numbers |   |  |  |
