import sys

def solve_case(s, words):
    if not words:
        return []
    wlen = len(words[0])
    total = wlen * len(words)
    target = sorted(words)
    ans = []
    for i in range(0, len(s) - total + 1):
        parts = [s[i + j * wlen:i + (j + 1) * wlen] for j in range(len(words))]
        if sorted(parts) == target:
            ans.append(i)
    return ans

def fmt(arr):
    return '[' + ','.join(str(x) for x in arr) + ']'

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    idx = 0
    t = int(lines[idx].strip()); idx += 1
    out = []
    for _ in range(t):
        s = lines[idx] if idx < len(lines) else ''; idx += 1
        m = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        words = []
        for _ in range(m):
            words.append(lines[idx] if idx < len(lines) else ''); idx += 1
        out.append(fmt(solve_case(s, words)))
    sys.stdout.write('\n'.join(out))
if __name__ == '__main__':
    main()
