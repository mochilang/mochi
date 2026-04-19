import sys


def ladder_length(begin, end, words):
    word_set = set(words)
    if end not in word_set:
        return 0
    level = {begin}
    visited = {begin}
    steps = 1
    while level:
        if end in level:
            return steps
        next_level = set()
        for word in sorted(level):
            chars = list(word)
            for i, orig in enumerate(chars):
                for c in "abcdefghijklmnopqrstuvwxyz":
                    if c == orig:
                        continue
                    chars[i] = c
                    nw = "".join(chars)
                    if nw in word_set and nw not in visited:
                        next_level.add(nw)
                chars[i] = orig
        visited |= next_level
        level = next_level
        steps += 1
    return 0


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        begin = lines[idx]
        idx += 1
        end = lines[idx]
        idx += 1
        n = int(lines[idx])
        idx += 1
        words = lines[idx : idx + n]
        idx += n
        out.append(str(ladder_length(begin, end, words)))
    sys.stdout.write("\n\n".join(out))
