import sys
from collections import defaultdict


def lcp(a: str, b: str) -> int:
    i = 0
    while i < len(a) and i < len(b) and a[i] == b[i]:
        i += 1
    return i


def abbreviate(word: str, prefix: int) -> str:
    if len(word) - prefix <= 2:
        return word
    return word[:prefix] + str(len(word) - prefix - 1) + word[-1]


def solve(words: list[str]) -> list[str]:
    groups: dict[tuple[int, str, str], list[tuple[str, int]]] = defaultdict(list)
    for i, word in enumerate(words):
        groups[(len(word), word[0], word[-1])].append((word, i))
    ans = [""] * len(words)
    for group in groups.values():
        group.sort()
        for j, (word, idx) in enumerate(group):
            need = 1
            if j > 0:
                need = max(need, lcp(word, group[j - 1][0]) + 1)
            if j + 1 < len(group):
                need = max(need, lcp(word, group[j + 1][0]) + 1)
            ans[idx] = abbreviate(word, need)
    return ans


def main() -> None:
    lines = [line.rstrip("\n") for line in sys.stdin]
    if not lines:
        return
    t = int(lines[0])
    idx = 1
    blocks: list[str] = []
    for tc in range(t):
        n = int(lines[idx])
        idx += 1
        words = lines[idx:idx + n]
        idx += n
        ans = solve(words)
        if tc:
            blocks.append("")
        blocks.append(str(len(ans)))
        blocks.extend(ans)
    sys.stdout.write("\n".join(blocks))


if __name__ == "__main__":
    main()
