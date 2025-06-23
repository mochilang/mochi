# Benchmarks

## math.fact_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 3 | best |
| Mochi (VM) | 11 | +266.7% |
| Mochi (Go) | 16 | +433.3% |
| Python (Cython) | 312 | +10300.0% |
| Typescript | 439 | +14533.3% |
| Python | 594 | +19700.0% |
| Python (PyPy) | 5326 | +177433.3% |
| Mochi (Interpreter) | 36630 | +1220900.0% |

## math.fact_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 7 | best |
| Mochi (Go) | 17 | +142.9% |
| Mochi (VM) | 22 | +214.3% |
| Python (Cython) | 872 | +12357.1% |
| Typescript | 944 | +13385.7% |
| Python | 1299 | +18457.1% |
| Python (PyPy) | 10857 | +155000.0% |
| Mochi (Interpreter) | 70613 | +1008657.1% |

## math.fact_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 9 | best |
| Mochi (Go) | 28 | +211.1% |
| Mochi (VM) | 30 | +233.3% |
| Typescript | 930 | +10233.3% |
| Python (Cython) | 1452 | +16033.3% |
| Python | 2434 | +26944.4% |
| Python (PyPy) | 21666 | +240633.3% |
| Mochi (Interpreter) | 158440 | +1760344.4% |

## math.fib_iter.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 3 | best |
| Mochi (Go) | 7 | +133.3% |
| Mochi (VM) | 12 | +300.0% |
| Python (Cython) | 253 | +8333.3% |
| Typescript | 291 | +9600.0% |
| Python | 397 | +13133.3% |
| Python (PyPy) | 3173 | +105666.7% |
| Mochi (Interpreter) | 29043 | +968000.0% |

## math.fib_iter.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 7 | best |
| Mochi (VM) | 13 | +85.7% |
| Mochi (Go) | 20 | +185.7% |
| Python (Cython) | 443 | +6228.6% |
| Typescript | 476 | +6700.0% |
| Python | 660 | +9328.6% |
| Python (PyPy) | 4394 | +62671.4% |
| Mochi (Interpreter) | 21548 | +307728.6% |

## math.fib_iter.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 9 | best |
| Mochi (Go) | 18 | +100.0% |
| Mochi (VM) | 23 | +155.6% |
| Python (Cython) | 641 | +7022.2% |
| Python | 1016 | +11188.9% |
| Typescript | 1064 | +11722.2% |
| Python (PyPy) | 7549 | +83777.8% |
| Mochi (Interpreter) | 66862 | +742811.1% |

## math.fib_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 0 | best |
| Mochi (Go) | 0 | best |
| C | 0 | best |
| Python (Cython) | 4 | ++Inf% |
| Python | 11 | ++Inf% |
| Typescript | 27 | ++Inf% |
| Python (PyPy) | 51 | ++Inf% |
| Mochi (Interpreter) | 2044 | ++Inf% |

## math.fib_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 16 | best |
| Mochi (VM) | 35 | +118.8% |
| Mochi (Go) | 35 | +118.8% |
| Python (Cython) | 476 | +2875.0% |
| Typescript | 555 | +3368.8% |
| Python | 1038 | +6387.5% |
| Python (PyPy) | 9312 | +58100.0% |
| Mochi (Interpreter) | 99491 | +621718.8% |

## math.fib_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 1572 | best |
| Mochi (VM) | 4387 | +179.1% |
| Mochi (Go) | 4407 | +180.3% |
| Typescript | 9807 | +523.9% |
| Python (PyPy) | 20762 | +1220.7% |
| Python (Cython) | 58645 | +3630.6% |
| Python | 125829 | +7904.4% |
| Mochi (Interpreter) | 11743675 | +746953.1% |

## math.mul_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 2 | best |
| Mochi (VM) | 6 | +200.0% |
| Mochi (Go) | 6 | +200.0% |
| Typescript | 287 | +14250.0% |
| Python (Cython) | 303 | +15050.0% |
| Python | 348 | +17300.0% |
| Python (PyPy) | 2239 | +111850.0% |
| Mochi (Interpreter) | 8582 | +429000.0% |

## math.mul_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 5 | best |
| Mochi (VM) | 12 | +140.0% |
| Mochi (Go) | 12 | +140.0% |
| Typescript | 412 | +8140.0% |
| Python (Cython) | 610 | +12100.0% |
| Python | 749 | +14880.0% |
| Python (PyPy) | 2211 | +44120.0% |
| Mochi (Interpreter) | 13660 | +273100.0% |

## math.mul_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 8 | best |
| Mochi (VM) | 18 | +125.0% |
| Mochi (Go) | 18 | +125.0% |
| Typescript | 670 | +8275.0% |
| Python (Cython) | 964 | +11950.0% |
| Python | 1151 | +14287.5% |
| Python (PyPy) | 3790 | +47275.0% |
| Mochi (Interpreter) | 18683 | +233437.5% |

## math.prime_count.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 1 | best |
| Mochi (VM) | 3 | +200.0% |
| Mochi (Go) | 3 | +200.0% |
| Typescript | 162 | +16100.0% |
| Python (Cython) | 165 | +16400.0% |
| Python | 197 | +19600.0% |
| Python (PyPy) | 951 | +95000.0% |
| Mochi (Interpreter) | 22700 | +2269900.0% |

## math.prime_count.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 8 | best |
| Mochi (Go) | 19 | +137.5% |
| Mochi (VM) | 23 | +187.5% |
| Typescript | 289 | +3512.5% |
| Python (Cython) | 449 | +5512.5% |
| Python | 551 | +6787.5% |
| Python (PyPy) | 6169 | +77012.5% |
| Mochi (Interpreter) | 26135 | +326587.5% |

## math.prime_count.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 17 | best |
| Mochi (Go) | 36 | +111.8% |
| Mochi (VM) | 45 | +164.7% |
| Python (Cython) | 732 | +4205.9% |
| Python | 894 | +5158.8% |
| Typescript | 1024 | +5923.5% |
| Mochi (Interpreter) | 31483 | +185094.1% |
| Python (PyPy) | 36291 | +213376.5% |

## math.sum_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (Go) | 6 | ++Inf% |
| Mochi (VM) | 8 | ++Inf% |
| Typescript | 232 | ++Inf% |
| Python (Cython) | 237 | ++Inf% |
| Python | 310 | ++Inf% |
| Python (PyPy) | 2063 | ++Inf% |
| Mochi (Interpreter) | 20624 | ++Inf% |

## math.sum_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (Go) | 12 | ++Inf% |
| Mochi (VM) | 15 | ++Inf% |
| Python (Cython) | 348 | ++Inf% |
| Typescript | 359 | ++Inf% |
| Python | 520 | ++Inf% |
| Python (PyPy) | 2028 | ++Inf% |
| Mochi (Interpreter) | 15977 | ++Inf% |

## math.sum_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (VM) | 18 | ++Inf% |
| Mochi (Go) | 18 | ++Inf% |
| Typescript | 474 | ++Inf% |
| Python (Cython) | 518 | ++Inf% |
| Python | 770 | ++Inf% |
| Python (PyPy) | 2311 | ++Inf% |
| Mochi (Interpreter) | 79435 | ++Inf% |

