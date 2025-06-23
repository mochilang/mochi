# Benchmarks

## math.fact_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 5 | best |
| Mochi (VM) | 9 | +80.0% |
| Mochi (Go) | 9 | +80.0% |
| Python (Cython) | 324 | +6380.0% |
| C | 332 | +6540.0% |
| Python | 605 | +12000.0% |
| Python (PyPy) | 5871 | +117320.0% |
| Mochi (Interpreter) | 88137 | +1762640.0% |

## math.fact_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 7 | best |
| Mochi (Go) | 17 | +142.9% |
| Mochi (VM) | 22 | +214.3% |
| Python (Cython) | 903 | +12800.0% |
| C | 957 | +13571.4% |
| Python | 1237 | +17571.4% |
| Python (PyPy) | 10709 | +152885.7% |
| Mochi (Interpreter) | 83593 | +1194085.7% |

## math.fact_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 7 | best |
| Mochi (VM) | 28 | +300.0% |
| Mochi (Go) | 28 | +300.0% |
| Python (Cython) | 1447 | +20571.4% |
| C | 1461 | +20771.4% |
| Python | 1989 | +28314.3% |
| Python (PyPy) | 23333 | +333228.6% |
| Mochi (Interpreter) | 146203 | +2088514.3% |

## math.fib_iter.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 5 | best |
| Mochi (VM) | 7 | +40.0% |
| Mochi (Go) | 7 | +40.0% |
| Python (Cython) | 276 | +5420.0% |
| C | 278 | +5460.0% |
| Python | 387 | +7640.0% |
| Python (PyPy) | 2770 | +55300.0% |
| Mochi (Interpreter) | 12751 | +254920.0% |

## math.fib_iter.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 6 | best |
| Mochi (Go) | 13 | +116.7% |
| Mochi (VM) | 16 | +166.7% |
| C | 475 | +7816.7% |
| Python (Cython) | 478 | +7866.7% |
| Python | 698 | +11533.3% |
| Python (PyPy) | 3445 | +57316.7% |
| Mochi (Interpreter) | 21519 | +358550.0% |

## math.fib_iter.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 6 | best |
| Mochi (Go) | 18 | +200.0% |
| Mochi (VM) | 23 | +283.3% |
| Python (Cython) | 694 | +11466.7% |
| C | 696 | +11500.0% |
| Python | 964 | +15966.7% |
| Python (PyPy) | 3084 | +51300.0% |
| Mochi (Interpreter) | 30276 | +504500.0% |

## math.fib_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 0 | best |
| Mochi (Go) | 0 | best |
| C | 4 | ++Inf% |
| Python (Cython) | 7 | ++Inf% |
| Python | 11 | ++Inf% |
| Python (PyPy) | 54 | ++Inf% |
| Typescript | 89 | ++Inf% |
| Mochi (Interpreter) | 733 | ++Inf% |

## math.fib_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (Go) | 35 | best |
| Mochi (VM) | 43 | +22.9% |
| Python (Cython) | 491 | +1302.9% |
| C | 502 | +1334.3% |
| Typescript | 651 | +1760.0% |
| Python | 1095 | +3028.6% |
| Python (PyPy) | 9930 | +28271.4% |
| Mochi (Interpreter) | 145639 | +416011.4% |

## math.fib_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (Go) | 4449 | best |
| Mochi (VM) | 4459 | +0.2% |
| Typescript | 10120 | +127.5% |
| Python (PyPy) | 30757 | +591.3% |
| C | 61224 | +1276.1% |
| Python (Cython) | 64706 | +1354.4% |
| Python | 131358 | +2852.5% |
| Mochi (Interpreter) | 12479499 | +280401.2% |

## math.mul_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 6 | best |
| Mochi (Go) | 6 | best |
| Typescript | 6 | best |
| C | 315 | +5150.0% |
| Python (Cython) | 318 | +5200.0% |
| Python | 346 | +5666.7% |
| Python (PyPy) | 2776 | +46166.7% |
| Mochi (Interpreter) | 8428 | +140366.7% |

## math.mul_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 12 | best |
| Mochi (Go) | 12 | best |
| Typescript | 39 | +225.0% |
| Python (Cython) | 663 | +5425.0% |
| C | 671 | +5491.7% |
| Python | 782 | +6416.7% |
| Python (PyPy) | 2391 | +19825.0% |
| Mochi (Interpreter) | 14346 | +119450.0% |

## math.mul_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 7 | best |
| Mochi (VM) | 18 | +157.1% |
| Mochi (Go) | 18 | +157.1% |
| C | 981 | +13914.3% |
| Python (Cython) | 986 | +13985.7% |
| Python | 1214 | +17242.9% |
| Python (PyPy) | 5251 | +74914.3% |
| Mochi (Interpreter) | 20711 | +295771.4% |

## math.prime_count.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 3 | best |
| Mochi (Go) | 3 | best |
| Typescript | 4 | +33.3% |
| C | 164 | +5366.7% |
| Python (Cython) | 164 | +5366.7% |
| Python | 237 | +7800.0% |
| Python (PyPy) | 1043 | +34666.7% |
| Mochi (Interpreter) | 9573 | +319000.0% |

## math.prime_count.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 4 | best |
| Mochi (VM) | 19 | +375.0% |
| Mochi (Go) | 19 | +375.0% |
| Python (Cython) | 434 | +10750.0% |
| C | 497 | +12325.0% |
| Python | 553 | +13725.0% |
| Python (PyPy) | 8237 | +205825.0% |
| Mochi (Interpreter) | 18284 | +457000.0% |

## math.prime_count.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 7 | best |
| Mochi (VM) | 36 | +414.3% |
| Mochi (Go) | 36 | +414.3% |
| Python (Cython) | 707 | +10000.0% |
| C | 753 | +10657.1% |
| Python | 932 | +13214.3% |
| Mochi (Interpreter) | 31245 | +446257.1% |
| Python (PyPy) | 68888 | +984014.3% |

## math.sum_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 5 | best |
| Mochi (Go) | 6 | +20.0% |
| Mochi (VM) | 8 | +60.0% |
| C | 244 | +4780.0% |
| Python (Cython) | 309 | +6080.0% |
| Python | 311 | +6120.0% |
| Python (PyPy) | 2253 | +44960.0% |
| Mochi (Interpreter) | 24445 | +488800.0% |

## math.sum_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 5 | best |
| Mochi (Go) | 12 | +140.0% |
| Mochi (VM) | 14 | +180.0% |
| Python (Cython) | 362 | +7140.0% |
| C | 388 | +7660.0% |
| Python | 614 | +12180.0% |
| Python (PyPy) | 3443 | +68760.0% |
| Mochi (Interpreter) | 13736 | +274620.0% |

## math.sum_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Typescript | 5 | best |
| Mochi (Go) | 18 | +260.0% |
| Mochi (VM) | 22 | +340.0% |
| Python (Cython) | 601 | +11920.0% |
| C | 687 | +13640.0% |
| Python | 884 | +17580.0% |
| Python (PyPy) | 2767 | +55240.0% |
| Mochi (Interpreter) | 19354 | +386980.0% |

