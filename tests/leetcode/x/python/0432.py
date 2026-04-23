import sys

class AllOne:
    def __init__(self) -> None:
        self.counts: dict[str, int] = {}

    def inc(self, key: str) -> None:
        self.counts[key] = self.counts.get(key, 0) + 1

    def dec(self, key: str) -> None:
        nxt = self.counts[key] - 1
        if nxt == 0:
            del self.counts[key]
        else:
            self.counts[key] = nxt

    def getMaxKey(self) -> str:
        if not self.counts:
            return ''
        m = max(self.counts.values())
        return min(k for k, v in self.counts.items() if v == m)

    def getMinKey(self) -> str:
        if not self.counts:
            return ''
        m = min(self.counts.values())
        return min(k for k, v in self.counts.items() if v == m)

def quote(value: str | None) -> str:
    return 'null' if value is None else '"' + value + '"'

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    cases: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        obj: AllOne | None = None
        out: list[str | None] = []
        for _ in range(n):
            op = data[idx]; idx += 1
            if op == 'C':
                obj = AllOne(); out.append(None)
            elif op == 'I':
                assert obj is not None
                obj.inc(data[idx]); idx += 1; out.append(None)
            elif op == 'D':
                assert obj is not None
                obj.dec(data[idx]); idx += 1; out.append(None)
            elif op == 'X':
                assert obj is not None
                out.append(obj.getMaxKey())
            else:
                assert obj is not None
                out.append(obj.getMinKey())
        cases.append('[' + ','.join(quote(x) for x in out) + ']')
    sys.stdout.write('\n\n'.join(cases))

if __name__ == '__main__':
    main()
