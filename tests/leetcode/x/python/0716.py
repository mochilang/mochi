import sys


class MaxStack:
    def __init__(self) -> None:
        self.stack: list[int] = []

    def push(self, x: int) -> None:
        self.stack.append(x)

    def pop(self) -> int:
        return self.stack.pop()

    def top(self) -> int:
        return self.stack[-1]

    def peek_max(self) -> int:
        return max(self.stack)

    def pop_max(self) -> int:
        max_value = max(self.stack)
        buffer: list[int] = []
        while self.stack[-1] != max_value:
            buffer.append(self.stack.pop())
        self.stack.pop()
        while buffer:
            self.stack.append(buffer.pop())
        return max_value


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    cases: list[str] = []
    for _ in range(t):
        ops = int(data[idx])
        idx += 1
        ms: MaxStack | None = None
        out: list[str] = []
        for _ in range(ops):
            op = data[idx]
            idx += 1
            if op == "C":
                ms = MaxStack()
                out.append("null")
            elif op == "P":
                assert ms is not None
                ms.push(int(data[idx]))
                idx += 1
                out.append("null")
            elif op == "O":
                assert ms is not None
                out.append(str(ms.pop()))
            elif op == "T":
                assert ms is not None
                out.append(str(ms.top()))
            elif op == "M":
                assert ms is not None
                out.append(str(ms.peek_max()))
            else:
                assert ms is not None
                out.append(str(ms.pop_max()))
        cases.append("[" + ",".join(out) + "]")
    sys.stdout.write("\n\n".join(cases))


if __name__ == "__main__":
    main()
