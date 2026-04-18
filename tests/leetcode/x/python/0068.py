import sys

def justify(words, max_width):
    res = []
    i = 0
    while i < len(words):
        j = i
        total = 0
        while j < len(words) and total + len(words[j]) + (j - i) <= max_width:
            total += len(words[j])
            j += 1
        gaps = j - i - 1
        if j == len(words) or gaps == 0:
            line = ' '.join(words[i:j])
            line += ' ' * (max_width - len(line))
        else:
            spaces = max_width - total
            base = spaces // gaps
            extra = spaces % gaps
            parts = []
            for k in range(i, j - 1):
                parts.append(words[k])
                parts.append(' ' * (base + (1 if k - i < extra else 0)))
            parts.append(words[j - 1])
            line = ''.join(parts)
        res.append(line)
        i = j
    return res

lines = sys.stdin.read().splitlines()
if lines:
    idx = 0
    t = int(lines[idx]); idx += 1
    out = []
    for tc in range(t):
        n = int(lines[idx]); idx += 1
        words = lines[idx:idx+n]; idx += n
        width = int(lines[idx]); idx += 1
        ans = justify(words, width)
        out.append(str(len(ans)))
        out.extend('|' + s + '|' for s in ans)
        if tc + 1 < t: out.append('=')
    sys.stdout.write('\n'.join(out))
