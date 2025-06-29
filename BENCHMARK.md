# Benchmarks

## join.hash_join.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 52011 | best |
| Mochi (Go) | 68409 | +31.5% |

## join.hash_join.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 193513 | best |
| Mochi (Go) | 217844 | +12.6% |

## join.hash_join.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (Go) | 511206 | best |
| Mochi (VM) | 552101 | +8.0% |

## join.hash_join_empty_right.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 0 | best |
| Mochi (Go) | 0 | best |

## join.hash_join_empty_right.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 1 | best |
| Mochi (Go) | 1 | best |

## join.hash_join_empty_right.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 1 | best |
| Mochi (Go) | 1 | best |

## join.nested_join.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (Go) | 50192 | best |
| Mochi (VM) | 55924 | +11.4% |

## join.nested_join.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (Go) | 213830 | best |
| Mochi (VM) | 572866 | +167.9% |

## join.nested_join.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 458039 | best |
| Mochi (Go) | 487328 | +6.4% |

## join.nested_join_empty_right.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 0 | best |
| Mochi (Go) | 0 | best |

## join.nested_join_empty_right.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 1 | best |
| Mochi (Go) | 1 | best |

## join.nested_join_empty_right.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 1 | best |
| Mochi (Go) | 3 | +200.0% |

## math.fact_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 4 | best |
| Mochi (VM) | 13 | +225.0% |
| Mochi (Go) | 13 | +225.0% |
| Typescript | 530 | +13150.0% |
| Python | 944 | +23500.0% |

## math.fact_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 9 | best |
| Mochi (VM) | 24 | +166.7% |
| Mochi (Go) | 25 | +177.8% |
| Typescript | 875 | +9622.2% |
| Python | 1762 | +19477.8% |

## math.fact_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 12 | best |
| Mochi (VM) | 39 | +225.0% |
| Mochi (Go) | 39 | +225.0% |
| Typescript | 1189 | +9808.3% |
| Python | 2743 | +22758.3% |

## math.fib_iter.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 5 | best |
| Mochi (VM) | 10 | +100.0% |
| Mochi (Go) | 10 | +100.0% |
| Typescript | 391 | +7720.0% |
| Python | 573 | +11360.0% |

## math.fib_iter.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 8 | best |
| Mochi (VM) | 18 | +125.0% |
| Mochi (Go) | 18 | +125.0% |
| Typescript | 515 | +6337.5% |
| Python | 950 | +11775.0% |

## math.fib_iter.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 13 | best |
| Mochi (VM) | 26 | +100.0% |
| Mochi (Go) | 26 | +100.0% |
| Typescript | 564 | +4238.5% |
| Python | 1329 | +10123.1% |

## math.fib_rec.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| Mochi (VM) | 0 | best |
| C | 0 | best |
| Mochi (Go) | 1 | ++Inf% |
| Python | 15 | ++Inf% |
| Typescript | 34 | ++Inf% |

## math.fib_rec.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 22 | best |
| Mochi (VM) | 49 | +122.7% |
| Mochi (Go) | 50 | +127.3% |
| Typescript | 670 | +2945.5% |
| Python | 1469 | +6577.3% |

## math.fib_rec.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 2057 | best |
| Mochi (Go) | 6134 | +198.2% |
| Mochi (VM) | 6209 | +201.8% |
| Typescript | 13486 | +555.6% |
| Python | 177258 | +8517.3% |

## math.mul_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 3 | best |
| Mochi (VM) | 9 | +200.0% |
| Mochi (Go) | 15 | +400.0% |
| Typescript | 338 | +11166.7% |
| Python | 526 | +17433.3% |

## math.mul_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 7 | best |
| Mochi (VM) | 17 | +142.9% |
| Mochi (Go) | 17 | +142.9% |
| Typescript | 548 | +7728.6% |
| Python | 1042 | +14785.7% |

## math.mul_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 11 | best |
| Mochi (VM) | 25 | +127.3% |
| Mochi (Go) | 25 | +127.3% |
| Typescript | 595 | +5309.1% |
| Python | 1613 | +14563.6% |

## math.prime_count.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 1 | best |
| Mochi (VM) | 4 | +300.0% |
| Mochi (Go) | 4 | +300.0% |
| Typescript | 211 | +21000.0% |
| Python | 311 | +31000.0% |

## math.prime_count.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 14 | best |
| Mochi (Go) | 26 | +85.7% |
| Mochi (VM) | 67 | +378.6% |
| Typescript | 378 | +2600.0% |
| Python | 767 | +5378.6% |

## math.prime_count.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 24 | best |
| Mochi (VM) | 50 | +108.3% |
| Mochi (Go) | 51 | +112.5% |
| Typescript | 833 | +3370.8% |
| Python | 1285 | +5254.2% |

## math.sum_loop.10
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (Go) | 15 | ++Inf% |
| Mochi (VM) | 16 | ++Inf% |
| Typescript | 455 | ++Inf% |
| Python | 466 | ++Inf% |

## math.sum_loop.20
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (VM) | 17 | ++Inf% |
| Mochi (Go) | 17 | ++Inf% |
| Typescript | 462 | ++Inf% |
| Python | 758 | ++Inf% |

## math.sum_loop.30
| Language | Time (µs) | +/- |
| --- | ---: | --- |
| C | 0 | best |
| Mochi (VM) | 25 | ++Inf% |
| Mochi (Go) | 25 | ++Inf% |
| Typescript | 554 | ++Inf% |
| Python | 1174 | ++Inf% |

