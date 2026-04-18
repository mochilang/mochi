import sys

def is_match(s, p):
    i = j = 0
    star = -1
    match = 0
    while i < len(s):
        if j < len(p) and (p[j] == '?' or p[j] == s[i]):
            i += 1
            j += 1
        elif j < len(p) and p[j] == '*':
            star = j
            match = i
            j += 1
        elif star != -1:
            j = star + 1
            match += 1
            i = match
        else:
            return False
    while j < len(p) and p[j] == '*':
        j += 1
    return j == len(p)

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    idx = 0
    t = int(lines[idx].strip()); idx += 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        s = lines[idx] if n > 0 else ""
        if n > 0: idx += 1
        m = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        p = lines[idx] if m > 0 else ""
        if m > 0: idx += 1
        out.append('true' if is_match(s, p) else 'false')
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__':
    main()
