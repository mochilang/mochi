import sys
from collections import defaultdict

def word_squares(words: list[str]) -> list[list[str]]:
    if not words:
        return []
    n = len(words[0])
    prefixes: dict[str, list[str]] = defaultdict(list)
    for word in words:
        for i in range(n + 1):
            prefixes[word[:i]].append(word)
    ans: list[list[str]] = []
    def backtrack(square: list[str]) -> None:
        depth = len(square)
        if depth == n:
            ans.append(square[:])
            return
        prefix = ''.join(row[depth] for row in square)
        for cand in prefixes.get(prefix, []):
            backtrack(square + [cand])
    for word in words:
        backtrack([word])
    return ans

def fmt(squares: list[list[str]]) -> str:
    return '[' + ','.join('[' + ','.join('"' + w + '"' for w in sq) + ']' for sq in squares) + ']'

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        words = data[idx:idx+n]; idx += n
        out.append(fmt(word_squares(words)))
    sys.stdout.write('\n\n'.join(out))

if __name__ == '__main__':
    main()
