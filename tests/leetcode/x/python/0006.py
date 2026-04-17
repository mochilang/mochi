import sys


def convert(s: str, num_rows: int) -> str:
    if num_rows <= 1 or num_rows >= len(s):
        return s
    cycle = 2 * num_rows - 2
    parts = []
    for row in range(num_rows):
        i = row
        while i < len(s):
            parts.append(s[i])
            diag = i + cycle - 2 * row
            if 0 < row < num_rows - 1 and diag < len(s):
                parts.append(s[diag])
            i += cycle
    return ''.join(parts)


def main() -> None:
    data = sys.stdin.read().splitlines()
    if not data:
        return
    t = int(data[0].strip())
    out = []
    idx = 1
    for _ in range(t):
        s = data[idx] if idx < len(data) else ''
        idx += 1
        num_rows = int(data[idx].strip()) if idx < len(data) else 1
        idx += 1
        out.append(convert(s, num_rows))
    sys.stdout.write('\n'.join(out))


if __name__ == '__main__':
    main()
