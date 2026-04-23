import sys

class RandomizedCollection:
    def __init__(self) -> None:
        self.values: list[int] = []
        self.pos: dict[int, set[int]] = {}

    def insert(self, val: int) -> bool:
        fresh = val not in self.pos or not self.pos[val]
        self.pos.setdefault(val, set()).add(len(self.values))
        self.values.append(val)
        return fresh

    def remove(self, val: int) -> bool:
        if val not in self.pos or not self.pos[val]:
            return False
        idx = max(self.pos[val])
        self.pos[val].remove(idx)
        last = self.values[-1]
        last_idx = len(self.values) - 1
        if idx != last_idx:
            self.values[idx] = last
            self.pos[last].remove(last_idx)
            self.pos[last].add(idx)
        self.values.pop()
        return True

    def get_random(self) -> int:
        return self.values[0]

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; cases: list[str] = []
    for _ in range(t):
        ops = int(data[idx]); idx += 1
        rc: RandomizedCollection | None = None
        out: list[str] = []
        for _ in range(ops):
            op = data[idx]; idx += 1
            if op == 'C':
                rc = RandomizedCollection(); out.append('null')
            elif op == 'I':
                assert rc is not None
                out.append('true' if rc.insert(int(data[idx])) else 'false'); idx += 1
            elif op == 'R':
                assert rc is not None
                out.append('true' if rc.remove(int(data[idx])) else 'false'); idx += 1
            else:
                assert rc is not None
                out.append(str(rc.get_random()))
        cases.append('[' + ','.join(out) + ']')
    sys.stdout.write('\n\n'.join(cases))
if __name__ == '__main__': main()
