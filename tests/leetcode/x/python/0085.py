import sys

def hist(h):
    best = 0
    for i in range(len(h)):
        mn = h[i]
        for j in range(i, len(h)):
            mn = min(mn, h[j])
            best = max(best, mn * (j - i + 1))
    return best

lines = sys.stdin.read().split()
if lines:
    idx = 0
    t = int(lines[idx]); idx += 1
    out = []
    for _ in range(t):
        rows = int(lines[idx]); idx += 1
        cols = int(lines[idx]); idx += 1
        h = [0] * cols
        best = 0
        for _ in range(rows):
            s = lines[idx]; idx += 1
            for c in range(cols):
                h[c] = h[c] + 1 if s[c] == '1' else 0
            best = max(best, hist(h))
        out.append(str(best))
    sys.stdout.write("\n".join(out))
