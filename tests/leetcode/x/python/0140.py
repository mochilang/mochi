import sys
from functools import lru_cache


def word_break(s, words):
    word_set = set(words)
    lengths = sorted({len(w) for w in words})

    @lru_cache(None)
    def dfs(i):
        if i == len(s):
            return [""]
        out = []
        for length in lengths:
            j = i + length
            if j > len(s):
                break
            word = s[i:j]
            if word in word_set:
                for tail in dfs(j):
                    out.append(word if tail == "" else word + " " + tail)
        out.sort()
        return out

    return dfs(0)


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        s = lines[idx]
        idx += 1
        n = int(lines[idx])
        idx += 1
        words = lines[idx:idx + n]
        idx += n
        ans = word_break(s, words)
        block = [str(len(ans))] + ans
        out.append("\n".join(block))
    sys.stdout.write("\n\n".join(out))
