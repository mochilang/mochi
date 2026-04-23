import sys

def quote(value: str | None) -> str:
    return 'null' if value is None else value

def simulate(capacity: int, ops: list[list[int | str]]) -> str:
    clock = 0
    cache: dict[int, tuple[int, int, int]] = {}
    out: list[str | None] = []
    for op in ops:
        clock += 1
        kind = op[0]
        if kind == 'C':
            cache = {}
            out.append(None)
        elif kind == 'P':
            key = int(op[1]); value = int(op[2])
            if capacity == 0:
                out.append(None)
                continue
            if key in cache:
                _, freq, _ = cache[key]
                cache[key] = (value, freq + 1, clock)
            else:
                if len(cache) == capacity:
                    victim = min(cache.items(), key=lambda kv: (kv[1][1], kv[1][2], kv[0]))[0]
                    del cache[victim]
                cache[key] = (value, 1, clock)
            out.append(None)
        else:
            key = int(op[1])
            if key not in cache:
                out.append('-1')
            else:
                value, freq, _ = cache[key]
                cache[key] = (value, freq + 1, clock)
                out.append(str(value))
    return '[' + ','.join(quote(x) for x in out) + ']'

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    cases: list[str] = []
    for _ in range(t):
        capacity = int(data[idx]); m = int(data[idx + 1]); idx += 2
        ops: list[list[int | str]] = []
        for _ in range(m):
            op = data[idx]; idx += 1
            if op == 'C':
                ops.append(['C'])
            elif op == 'P':
                ops.append(['P', int(data[idx]), int(data[idx + 1])])
                idx += 2
            else:
                ops.append(['G', int(data[idx])])
                idx += 1
        cases.append(simulate(capacity, ops))
    sys.stdout.write('\n\n'.join(cases))

if __name__ == '__main__':
    main()
