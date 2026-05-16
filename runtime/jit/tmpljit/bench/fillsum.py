"""Reference workload for crosslang comparison with the MEP-30 JIT.

Computes sum_{i=0..n-1} (i*2 + 3). Measured with time.perf_counter_ns()
over a calibrated outer loop sized so total runtime >= 1 second; reports
ns/op (per call to fillsum)."""

import sys
import time


def fillsum(n: int) -> int:
    s = 0
    for i in range(n):
        s += i * 2 + 3
    return s


def bench(n: int) -> None:
    # Calibration: find iters such that total runtime is >= 1s.
    iters = 1
    while True:
        t0 = time.perf_counter_ns()
        for _ in range(iters):
            fillsum(n)
        dt = time.perf_counter_ns() - t0
        if dt >= 1_000_000_000:
            ns = dt / iters
            print(f"fillsum_py n={n}: {ns:10.1f} ns/op  ({iters} iters, {dt/1e9:.2f}s)")
            return
        iters *= 2


def main() -> int:
    for n in (128, 1024, 10000):
        bench(n)
    return 0


if __name__ == "__main__":
    sys.exit(main())
