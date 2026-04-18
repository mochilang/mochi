import sys

def match_at(s, p, i, j):
    if j == len(p):
        return i == len(s)
    first = i < len(s) and (p[j] == '.' or s[i] == p[j])
    if j + 1 < len(p) and p[j + 1] == '*':
        return match_at(s, p, i, j + 2) or (first and match_at(s, p, i + 1, j))
    return first and match_at(s, p, i + 1, j + 1)

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        s = lines[idx] if idx < len(lines) else ''
        idx += 1
        p = lines[idx] if idx < len(lines) else ''
        idx += 1
        out.append('true' if match_at(s, p, 0, 0) else 'false')
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__':
    main()
