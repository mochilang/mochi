import sys
from collections import defaultdict

def ladders(begin, end, words):
    word_set = set(words)
    if end not in word_set:
        return []
    parents = defaultdict(list)
    level = {begin}
    visited = {begin}
    found = False
    while level and not found:
        next_level = set()
        local_seen = set()
        for word in sorted(level):
            arr = list(word)
            for i, orig in enumerate(arr):
                for c in 'abcdefghijklmnopqrstuvwxyz':
                    if c == orig:
                        continue
                    arr[i] = c
                    nw = ''.join(arr)
                    if nw in word_set and nw not in visited:
                        if nw not in local_seen:
                            next_level.add(nw)
                            local_seen.add(nw)
                        parents[nw].append(word)
                        if nw == end:
                            found = True
                arr[i] = orig
        visited |= next_level
        level = next_level
    if not found:
        return []
    out = []
    path = [end]
    def backtrack(word):
        if word == begin:
            out.append(path[::-1])
            return
        for p in sorted(parents[word]):
            path.append(p)
            backtrack(p)
            path.pop()
    backtrack(end)
    out.sort()
    return out

def fmt(paths):
    lines = [str(len(paths))]
    for p in paths:
        lines.append('->'.join(p))
    return '\n'.join(lines)

lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0]); idx = 1; out = []
    for _ in range(tc):
        begin = lines[idx]; idx += 1
        end = lines[idx]; idx += 1
        n = int(lines[idx]); idx += 1
        words = lines[idx:idx+n]; idx += n
        out.append(fmt(ladders(begin, end, words)))
    sys.stdout.write('\n\n'.join(out))
