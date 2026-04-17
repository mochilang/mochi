import sys

def solve(lines):
    if not lines:
        return ''
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        k = int(lines[idx].strip()) if idx < len(lines) else 0
        idx += 1
        vals = []
        for _ in range(k):
            n = int(lines[idx].strip()) if idx < len(lines) else 0
            idx += 1
            for _ in range(n):
                vals.append(int(lines[idx].strip()) if idx < len(lines) else 0)
                idx += 1
        vals.sort()
        out.append('[' + ','.join(str(v) for v in vals) + ']')
    return '\n'.join(out)

def main():
    sys.stdout.write(solve(sys.stdin.read().splitlines()))

if __name__ == '__main__':
    main()
