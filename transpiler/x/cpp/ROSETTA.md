# C++ Transpiler Rosetta Output

This directory stores C++ code generated from Mochi programs in `tests/rosetta/x/Mochi`. Each file is compiled and executed during tests. Successful runs keep the generated `.cpp` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (437/491) - Last updated 2025-08-16 13:47 +0700:
| Index | Name | Status | Duration | Memory |
| ---: | --- | :---: | ---: | ---: |
| 1 | 100-doors-2 | ✓ | 273.0µs | 3.62MB |
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
| 51 | animation | ✓ | 645.0µs | 3.45MB |
| 52 | anonymous-recursion-1 | ✓ | 199.0µs | 3.60MB |
| 53 | anonymous-recursion-2 | ✓ | 240.0µs | 3.66MB |
| 54 | anonymous-recursion | ✓ | 212.0µs | 3.68MB |
| 55 | anti-primes | ✓ | 43.0ms | 3.74MB |
| 56 | append-a-record-to-the-end-of-a-text-file | ✓ | 139.0µs | 3.46MB |
| 57 | apply-a-callback-to-an-array-1 | ✓ | 215.0µs | 3.73MB |
| 58 | apply-a-callback-to-an-array-2 | ✓ | 323.0µs | 3.73MB |
| 59 | apply-a-digital-filter-direct-form-ii-transposed- | ✓ | 373.0µs | 3.80MB |
| 60 | approximate-equality | ✓ | 297.0µs | 3.75MB |
| 61 | arbitrary-precision-integers-included- | ✓ | 163.0µs | 4.39MB |
| 62 | archimedean-spiral | ✓ | 1.0ms | 3.73MB |
| 63 | arena-storage-pool | ✓ | 211.0µs | 3.55MB |
| 64 | arithmetic-complex | ✓ | 329.0µs | 3.78MB |
| 65 | arithmetic-derivative | ✓ | 1.0ms | 3.64MB |
| 66 | arithmetic-evaluation | ✓ | 226.0µs | 3.52MB |
| 67 | arithmetic-geometric-mean-calculate-pi | ✓ | 346.0µs | 3.72MB |
| 68 | arithmetic-geometric-mean | ✓ | 331.0µs | 3.75MB |
| 69 | arithmetic-integer-1 | ✓ | 156.0µs | 3.47MB |
| 70 | arithmetic-integer-2 | ✓ | 154.0µs | 3.54MB |
| 71 | arithmetic-numbers |   |  |  |
| 72 | arithmetic-rational | ✓ | 236.0µs | 3.46MB |
| 73 | array-concatenation | ✓ | 203.0µs | 3.55MB |
| 74 | array-length | ✓ | 190.0µs | 3.59MB |
| 75 | arrays | ✓ | 252.0µs | 3.62MB |
| 76 | ascending-primes | ✓ | 592.0µs | 3.68MB |
| 77 | ascii-art-diagram-converter | ✓ | 171.0µs | 3.70MB |
| 78 | assertions | ✓ | 144.0µs | 3.43MB |
| 79 | associative-array-creation | ✓ | 187.0µs | 3.54MB |
| 80 | associative-array-iteration | ✓ | 246.0µs | 3.64MB |
| 81 | associative-array-merging | ✓ | 380.0µs | 3.83MB |
| 82 | atomic-updates | ✓ | 307.0µs | 3.69MB |
| 83 | attractive-numbers | ✓ | 311.0µs | 3.81MB |
| 84 | average-loop-length | ✓ | 1.27s | 3.79MB |
| 85 | averages-arithmetic-mean | ✓ | 480.0µs | 3.72MB |
| 86 | averages-mean-time-of-day | ✓ | 247.0µs | 3.47MB |
| 87 | averages-median-1 | ✓ | 310.0µs | 3.66MB |
| 88 | averages-median-2 | ✓ | 371.0µs | 3.72MB |
| 89 | averages-median-3 | ✓ | 342.0µs | 3.85MB |
| 90 | averages-mode | ✓ | 189.0µs | 3.77MB |
| 91 | averages-pythagorean-means | ✓ | 444.0µs | 3.69MB |
| 92 | averages-root-mean-square | ✓ | 229.0µs | 3.66MB |
| 93 | averages-simple-moving-average | ✓ | 453.0µs | 3.79MB |
| 94 | avl-tree |   |  |  |
| 95 | b-zier-curves-intersections | ✓ | 14.0ms | 3.95MB |
| 96 | babbage-problem | ✓ | 288.0µs | 3.64MB |
| 97 | babylonian-spiral | ✓ | 2.0ms | 3.67MB |
| 98 | balanced-brackets |   | 439.0µs | 3.42MB |
| 99 | balanced-ternary | ✓ | 265.0µs | 3.71MB |
| 100 | barnsley-fern | ✓ | 100.0ms | 4.07MB |
| 101 | base64-decode-data | ✓ | 63.0µs | 3.62MB |
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
| 205 | circular-primes | ✓ | 887.0µs | 3.66MB |
| 206 | cistercian-numerals | ✓ | 20.0ms | 3.83MB |
| 207 | comma-quibbling | ✓ | 209.0µs | 3.64MB |
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
| 229 | constrained-random-points-on-a-circle-1 | ✓ | 29.0ms | 3.81MB |
| 230 | constrained-random-points-on-a-circle-2 | ✓ | 33.0ms | 3.82MB |
| 231 | continued-fraction | ✓ | 925.0µs | 3.67MB |
| 232 | convert-decimal-number-to-rational | ✓ | 285.0µs | 3.49MB |
| 233 | convert-seconds-to-compound-duration | ✓ | 262.0µs | 3.75MB |
| 234 | convex-hull | ✓ | 212.0µs | 3.77MB |
| 235 | conways-game-of-life | ✓ | 16.49s | 3.66MB |
| 236 | copy-a-string-1 | ✓ | 1.0µs | 3.45MB |
| 237 | copy-a-string-2 | ✓ | 168.0µs | 3.65MB |
| 238 | copy-stdin-to-stdout-1 | ✓ | 184.0µs | 3.41MB |
| 239 | copy-stdin-to-stdout-2 | ✓ | 272.0µs | 3.41MB |
| 240 | count-in-factors | ✓ | 565.0µs | 3.48MB |
| 241 | count-in-octal-1 | ✓ | 427.0µs | 3.69MB |
| 242 | count-in-octal-2 | ✓ | 244.0ms | 3.57MB |
| 243 | count-in-octal-3 | ✓ | 272.0µs | 3.62MB |
| 244 | count-in-octal-4 | ✓ | 248.0µs | 3.53MB |
| 245 | count-occurrences-of-a-substring | ✓ | 384.0µs | 3.54MB |
| 246 | count-the-coins-1 | ✓ | 295.0µs | 3.75MB |
| 247 | count-the-coins-2 | ✓ | 1.0ms | 3.82MB |
| 248 | cramers-rule | ✓ | 826.0µs | 3.68MB |
| 249 | crc-32-1 | ✓ | 231.0µs | 3.59MB |
| 250 | crc-32-2 | ✓ | 213.0µs | 3.54MB |
| 251 | create-a-file-on-magnetic-tape | ✓ | 79.0µs | 3.46MB |
| 252 | create-a-file | ✓ | 235.0µs | 3.58MB |
| 253 | create-a-two-dimensional-array-at-runtime-1 | ✓ | 249.0µs | 3.61MB |
| 254 | create-an-html-table | ✓ | 336.0µs | 3.62MB |
| 255 | create-an-object-at-a-given-address | ✓ | 335.0µs | 3.54MB |
| 256 | csv-data-manipulation | ✓ | 257.0µs | 3.58MB |
| 257 | csv-to-html-translation-1 | ✓ | 265.0µs | 3.53MB |
| 258 | csv-to-html-translation-2 | ✓ | 233.0µs | 3.54MB |
| 259 | csv-to-html-translation-3 | ✓ | 190.0µs | 3.46MB |
| 260 | csv-to-html-translation-4 | ✓ | 216.0µs | 3.46MB |
| 261 | csv-to-html-translation-5 | ✓ | 497.0µs | 3.54MB |
| 262 | cuban-primes |   |  |  |
| 263 | cullen-and-woodall-numbers | ✓ | 891.0µs | 3.82MB |
| 264 | cumulative-standard-deviation | ✓ | 455.0µs | 3.67MB |
| 265 | currency | ✓ | 333.0µs | 3.64MB |
| 266 | currying |   |  |  |
| 267 | curzon-numbers | ✓ | 17.0ms | 3.76MB |
| 268 | cusip | ✓ | 257.0µs | 3.49MB |
| 269 | cyclops-numbers |   |  |  |
| 270 | damm-algorithm | ✓ | 427.0µs | 3.62MB |
| 271 | date-format | ✓ | 299.0µs | 3.62MB |
| 272 | date-manipulation |   |  |  |
| 273 | day-of-the-week | ✓ | 262.0µs | 3.53MB |
| 274 | de-bruijn-sequences |   |  |  |
| 275 | deal-cards-for-freecell |   |  |  |
| 276 | death-star | ✓ | 17.0ms | 3.59MB |
| 277 | deceptive-numbers | ✓ | 28.0ms | 3.86MB |
| 278 | deconvolution-1d-2 | ✓ | 622.0µs | 3.74MB |
| 279 | deconvolution-1d-3 | ✓ | 465.0µs | 3.71MB |
| 280 | deconvolution-1d | ✓ | 389.0µs | 3.61MB |
| 281 | deepcopy-1 |   |  |  |
| 282 | define-a-primitive-data-type |   |  |  |
| 283 | delegates |   |  |  |
| 284 | demings-funnel |   |  |  |
| 285 | department-numbers | ✓ | 324.0µs | 3.61MB |
| 286 | descending-primes | ✓ | 636.0µs | 3.57MB |
| 287 | detect-division-by-zero | ✓ |  |  |
| 288 | determine-if-a-string-has-all-the-same-characters | ✓ | 377.0µs | 3.61MB |
| 289 | determine-if-a-string-has-all-unique-characters | ✓ | 520.0µs | 3.64MB |
| 290 | determine-if-a-string-is-collapsible | ✓ | 677.0µs | 3.71MB |
| 291 | determine-if-a-string-is-numeric-1 | ✓ | 416.0µs | 3.69MB |
| 292 | determine-if-a-string-is-numeric-2 | ✓ | 288.0µs | 3.68MB |
| 293 | determine-if-a-string-is-squeezable | ✓ | 765.0µs | 3.70MB |
| 294 | determine-if-only-one-instance-is-running | ✓ | 209.0µs | 3.56MB |
| 295 | determine-if-two-triangles-overlap | ✓ | 721.0µs | 3.86MB |
| 296 | determine-sentence-type | ✓ | 337.0µs | 3.51MB |
| 297 | dice-game-probabilities-1 | ✓ | 125.0ms | 3.65MB |
| 298 | dice-game-probabilities-2 | ✓ | 781.0µs | 3.73MB |
| 299 | digital-root-multiplicative-digital-root | ✓ | 57.0ms | 3.70MB |
| 300 | dijkstras-algorithm | ✓ | 538.0µs | 3.54MB |
| 301 | dinesmans-multiple-dwelling-problem | ✓ | 236.0µs | 3.52MB |
| 302 | dining-philosophers-1 |   |  |  |
| 303 | dining-philosophers-2 |   |  |  |
| 304 | disarium-numbers | ✓ | 6.62s | 3.49MB |
| 305 | discordian-date | ✓ | 389.0µs | 3.70MB |
| 306 | display-a-linear-combination | ✓ | 325.0µs | 3.48MB |
| 307 | display-an-outline-as-a-nested-table |   |  |  |
| 308 | distance-and-bearing |   |  |  |
| 309 | distributed-programming |   |  |  |
| 310 | diversity-prediction-theorem |   |  |  |
| 311 | dns-query |   |  |  |
| 312 | documentation |   |  |  |
| 313 | doomsday-rule |   |  |  |
| 314 | dot-product |   |  |  |
| 315 | doubly-linked-list-definition-1 | ✓ | 2.0µs | 3.34MB |
| 316 | doubly-linked-list-definition-2 |   |  |  |
| 317 | doubly-linked-list-element-definition |   |  |  |
| 318 | doubly-linked-list-traversal | ✓ | 220.0µs | 3.58MB |
| 319 | dragon-curve | ✓ | 1.0ms | 3.70MB |
| 320 | draw-a-clock | ✓ | 142.0µs | 3.57MB |
| 321 | draw-a-cuboid | ✓ | 965.0µs | 3.53MB |
| 322 | draw-a-pixel-1 | ✓ | 3.04s | 8.25MB |
| 323 | draw-a-rotating-cube | ✓ | 89.0ms | 3.76MB |
| 324 | draw-a-sphere | ✓ | 3.0ms | 3.63MB |
| 325 | duffinian-numbers | ✓ | 65.0ms | 3.70MB |
| 326 | dutch-national-flag-problem | ✓ | 223.0µs | 3.60MB |
| 327 | dynamic-variable-names | ✓ | 252.0µs | 3.77MB |
| 328 | earliest-difference-between-prime-gaps | ✓ | 353.0µs | 3.56MB |
| 329 | eban-numbers | ✓ | 3.0ms | 3.64MB |
| 330 | ecdsa-example | ✓ | 161.0µs | 3.70MB |
| 331 | echo-server | ✓ | 171.0µs | 3.53MB |
| 332 | eertree |   |  |  |
| 333 | egyptian-division | ✓ | 171.0µs | 3.48MB |
| 334 | ekg-sequence-convergence | ✓ | 5.0ms | 3.49MB |
| 335 | element-wise-operations | ✓ | 1.0ms | 3.66MB |
| 336 | elementary-cellular-automaton-infinite-length | ✓ | 2.0ms | 3.48MB |
| 337 | elementary-cellular-automaton-random-number-generator | ✓ |  |  |
| 338 | elementary-cellular-automaton | ✓ | 678.0µs | 3.48MB |
| 339 | elliptic-curve-arithmetic | ✓ | 319.0µs | 3.78MB |
| 340 | elliptic-curve-digital-signature-algorithm | ✓ | 219.0µs | 3.46MB |
| 341 | emirp-primes | ✓ | 451.0ms | 3.91MB |
| 342 | empty-directory | ✓ | 227.0µs | 3.70MB |
| 343 | empty-program | ✓ | 13.0µs | 3.56MB |
| 344 | empty-string-1 | ✓ | 177.0µs | 3.56MB |
| 345 | empty-string-2 | ✓ | 280.0µs | 3.50MB |
| 346 | enforced-immutability | ✓ | 130.0µs | 3.54MB |
| 347 | entropy-1 | ✓ | 407.0µs | 3.78MB |
| 348 | entropy-2 | ✓ | 395.0µs | 3.84MB |
| 349 | entropy-narcissist | ✓ | 2.0ms | 3.75MB |
| 350 | enumerations-1 | ✓ | 2.0µs | 3.43MB |
| 351 | enumerations-2 | ✓ | 2.0µs | 3.50MB |
| 352 | enumerations-3 |   |  |  |
| 353 | enumerations-4 |   |  |  |
| 354 | environment-variables-1 |   |  |  |
| 355 | environment-variables-2 |   |  |  |
| 356 | equal-prime-and-composite-sums | ✓ | 1.79s | 4.04MB |
| 357 | equilibrium-index | ✓ | 288.0ms | 3.88MB |
| 358 | erd-s-nicolas-numbers |   |  |  |
| 359 | erd-s-selfridge-categorization-of-primes | ✓ | 10.0ms | 3.84MB |
| 360 | esthetic-numbers | ✓ | 1.45s | 4.91MB |
| 361 | ethiopian-multiplication | ✓ | 184.0µs | 3.68MB |
| 362 | euclid-mullin-sequence | ✓ | 48.0ms | 3.61MB |
| 363 | euler-method | ✓ | 1.0ms | 3.74MB |
| 364 | eulers-constant-0.5772... | ✓ | 573.0µs | 3.82MB |
| 365 | eulers-identity | ✓ | 365.0µs | 3.61MB |
| 366 | eulers-sum-of-powers-conjecture | ✓ | 596.0ms | 6.93MB |
| 367 | evaluate-binomial-coefficients | ✓ | 260.0µs | 3.81MB |
| 368 | even-or-odd | ✓ | 327.0µs | 3.75MB |
| 369 | events | ✓ | 182.0µs | 3.63MB |
| 370 | evolutionary-algorithm | ✓ | 96.0ms | 3.53MB |
| 371 | exceptions-catch-an-exception-thrown-in-a-nested-call | ✓ | 165.0µs | 3.46MB |
| 372 | exceptions | ✓ | 231.0µs | 3.62MB |
| 373 | executable-library | ✓ | 3.75s | 3.84MB |
| 374 | execute-a-markov-algorithm | ✓ | 7.0ms | 3.59MB |
| 375 | execute-a-system-command | ✓ | 80.0µs | 3.58MB |
| 376 | execute-brain- | ✓ | 718.0µs | 3.66MB |
| 377 | execute-computer-zero-1 | ✓ | 2.0ms | 3.75MB |
| 378 | execute-computer-zero | ✓ | 245.0µs | 3.61MB |
| 379 | execute-hq9+ | ✓ | 1.0ms | 3.63MB |
| 380 | execute-snusp | ✓ | 196.0µs | 3.41MB |
| 381 | exponentiation-operator-2 | ✓ | 634.0µs | 3.84MB |
| 382 | exponentiation-operator | ✓ | 800.0µs | 3.68MB |
| 383 | exponentiation-order | ✓ | 370.0µs | 3.68MB |
| 384 | exponentiation-with-infix-operators-in-or-operating-on-the-base | ✓ | 287.0µs | 3.61MB |
| 385 | extend-your-language | ✓ | 325.0µs | 3.57MB |
| 386 | extensible-prime-generator | ✓ | 307.0ms | 3.73MB |
| 387 | extreme-floating-point-values | ✓ | 689.0µs | 3.84MB |
| 388 | faces-from-a-mesh-2 | ✓ | 470.0µs | 3.70MB |
| 389 | faces-from-a-mesh | ✓ | 487.0µs | 3.66MB |
| 390 | factorial-base-numbers-indexing-permutations-of-a-collection |   |  |  |
| 391 | factorial-primes | ✓ | 976.0µs | 3.67MB |
| 392 | factorial | ✓ | 1.0ms | 3.76MB |
| 393 | factorions | ✓ | 321.0ms | 3.69MB |
| 394 | factors-of-a-mersenne-number | ✓ | 176.0ms | 3.74MB |
| 395 | factors-of-an-integer | ✓ | 2.06s | 3.54MB |
| 396 | fairshare-between-two-and-more | ✓ | 3.82s | 3.80MB |
| 397 | farey-sequence | ✓ | 1.0ms | 3.72MB |
| 398 | fast-fourier-transform | ✓ | 471.0µs | 3.61MB |
| 399 | fasta-format | ✓ | 260.0µs | 3.61MB |
| 400 | faulhabers-formula |   |  |  |
| 401 | faulhabers-triangle |   |  |  |
| 402 | feigenbaum-constant-calculation | ✓ | 1.0ms | 3.78MB |
| 403 | fermat-numbers |   |  |  |
| 404 | fibonacci-n-step-number-sequences |   |  |  |
| 405 | fibonacci-sequence-1 | ✓ | 1.0µs | 3.45MB |
| 406 | fibonacci-sequence-2 |   |  |  |
| 407 | fibonacci-sequence-3 | ✓ | 2.0µs | 3.56MB |
| 408 | fibonacci-sequence-4 | ✓ | 252.0µs | 3.58MB |
| 409 | fibonacci-sequence-5 | ✓ | 404.0µs | 3.63MB |
| 410 | fibonacci-word-fractal | ✓ | 1.0ms | 3.80MB |
| 411 | fibonacci-word | ✓ |  |  |
| 412 | file-extension-is-in-extensions-list | ✓ | 447.0µs | 3.64MB |
| 413 | file-input-output-1 | ✓ | 120.0µs | 3.64MB |
| 414 | file-input-output-2 | ✓ | 278.0µs | 3.60MB |
| 415 | file-input-output-3 | ✓ | 152.0µs | 3.44MB |
| 416 | file-modification-time | ✓ | 216.0µs | 3.54MB |
| 417 | file-size-distribution | ✓ | 282.0µs | 3.54MB |
| 418 | file-size | ✓ | 222.0µs | 3.69MB |
| 419 | filter | ✓ | 303.0µs | 3.61MB |
| 420 | find-chess960-starting-position-identifier-2 | ✓ | 380.0µs | 3.72MB |
| 421 | find-chess960-starting-position-identifier | ✓ | 341.0µs | 3.51MB |
| 422 | find-common-directory-path | ✓ | 448.0µs | 3.49MB |
| 423 | find-duplicate-files | ✓ | 194.0µs | 3.59MB |
| 424 | find-largest-left-truncatable-prime-in-a-given-base | ✓ | 4.0ms | 3.74MB |
| 425 | find-limit-of-recursion | ✓ | 579.0µs | 3.69MB |
| 426 | find-palindromic-numbers-in-both-binary-and-ternary-bases | ✓ | 2.12s | 3.63MB |
| 427 | find-the-intersection-of-a-line-with-a-plane | ✓ | 383.0µs | 3.84MB |
| 428 | find-the-intersection-of-two-lines | ✓ | 305.0µs | 3.76MB |
| 429 | find-the-last-sunday-of-each-month | ✓ | 334.0µs | 3.75MB |
| 430 | find-the-missing-permutation | ✓ | 199.0µs | 3.64MB |
| 431 | first-class-environments | ✓ | 977.0µs | 3.61MB |
| 432 | first-class-functions-use-numbers-analogously | ✓ | 395.0µs | 3.73MB |
| 433 | first-power-of-2-that-has-leading-decimal-digits-of-12 | ✓ | 304.0µs | 3.61MB |
| 434 | five-weekends | ✓ | 652.0µs | 3.53MB |
| 435 | fivenum-1 | ✓ | 413.0µs | 3.70MB |
| 436 | fivenum-2 | ✓ | 480.0µs | 3.84MB |
| 437 | fivenum-3 | ✓ | 334.0µs | 3.83MB |
| 438 | fixed-length-records-1 | ✓ | 245.0µs | 3.48MB |
| 439 | fixed-length-records-2 | ✓ | 192.0µs | 3.64MB |
| 440 | fizzbuzz-1 | ✓ | 261.0µs | 3.62MB |
| 441 | fizzbuzz-2 | ✓ | 928.0µs | 3.56MB |
| 442 | fizzbuzz | ✓ | 325.0µs | 3.63MB |
| 443 | flatten-a-list-1 | ✓ | 170.0µs | 3.56MB |
| 444 | flatten-a-list-2 | ✓ | 222.0µs | 3.48MB |
| 445 | flipping-bits-game | ✓ | 295.0µs | 3.51MB |
| 446 | flow-control-structures-1 | ✓ | 174.0µs | 3.57MB |
| 447 | flow-control-structures-2 | ✓ | 167.0µs | 3.62MB |
| 448 | flow-control-structures-3 | ✓ | 186.0µs | 3.52MB |
| 449 | flow-control-structures-4 | ✓ | 154.0µs | 3.45MB |
| 450 | floyd-warshall-algorithm | ✓ | 338.0µs | 3.71MB |
| 451 | floyd-warshall-algorithm2 |   |  |  |
| 452 | floyds-triangle | ✓ | 323.0µs | 3.48MB |
| 453 | forest-fire | ✓ | 4.0ms | 3.62MB |
| 454 | fork-2 | ✓ | 258.0µs | 3.46MB |
| 455 | fork | ✓ | 182.0µs | 3.50MB |
| 456 | formal-power-series | ✓ | 724.0µs | 3.79MB |
| 457 | formatted-numeric-output | ✓ | 186.0µs | 3.48MB |
| 458 | forward-difference | ✓ | 279.0µs | 3.74MB |
| 459 | four-bit-adder-1 | ✓ | 164.0µs | 3.54MB |
| 460 | four-is-magic | ✓ | 343.0µs | 3.44MB |
| 461 | four-is-the-number-of-letters-in-the-... |   |  |  |
| 462 | fractal-tree | ✓ | 203.0ms | 3.97MB |
| 463 | fractran | ✓ | 6.87s | 3.57MB |
| 464 | french-republican-calendar | ✓ | 222.0µs | 3.70MB |
| 465 | ftp | ✓ | 206.0µs | 3.64MB |
| 466 | function-frequency | ✓ | 1.0ms | 3.80MB |
| 467 | function-prototype | ✓ | 7.0µs | 3.44MB |
| 468 | functional-coverage-tree |   |  |  |
| 469 | fusc-sequence | ✓ | 366.0µs | 3.56MB |
| 470 | gamma-function | ✓ | 372.0µs | 3.70MB |
| 471 | general-fizzbuzz | ✓ | 215.0µs | 3.62MB |
| 472 | generic-swap |   |  |  |
| 473 | get-system-command-output | ✓ | 183.0µs | 3.56MB |
| 474 | giuga-numbers | ✓ | 193.0µs | 3.43MB |
| 475 | globally-replace-text-in-several-files | ✓ | 171.0µs | 3.68MB |
| 476 | goldbachs-comet | ✓ | 1.0ms | 3.62MB |
| 477 | golden-ratio-convergence | ✓ | 360.0µs | 3.82MB |
| 478 | graph-colouring | ✓ | 150.0µs | 3.48MB |
| 479 | gray-code | ✓ | 556.0µs | 3.75MB |
| 480 | gui-component-interaction | ✓ | 225.0µs | 3.70MB |
| 481 | gui-enabling-disabling-of-controls | ✓ | 339.0µs | 3.46MB |
| 482 | gui-maximum-window-dimensions | ✓ | 189.0µs | 3.72MB |
| 483 | http | ✓ | 146.0µs | 3.57MB |
| 484 | image-noise | ✓ | 65.0ms | 3.87MB |
| 485 | loops-increment-loop-index-within-loop-body | ✓ | 50.0ms | 3.68MB |
| 486 | md5 | ✓ | 220.0µs | 4.99MB |
| 487 | nim-game | ✓ | 284.0µs | 3.70MB |
| 488 | plasma-effect | ✓ | 4.0ms | 3.57MB |
| 489 | sorting-algorithms-bubble-sort | ✓ | 216.0µs | 3.43MB |
| 490 | window-management | ✓ | 281.0µs | 3.60MB |
| 491 | zumkeller-numbers | ✓ | 2.74s | 3.63MB |
