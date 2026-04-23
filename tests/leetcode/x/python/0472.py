import sys


def can_form(word: str, seen: set[str]) -> bool:
    if not seen:
        return False
    dp = [False] * (len(word) + 1)
    dp[0] = True
    for i in range(1, len(word) + 1):
        for j in range(i):
            if dp[j] and word[j:i] in seen:
                dp[i] = True
                break
    return dp[-1]


def fmt(words: list[str]) -> str:
    return "[" + ",".join(f'"{word}"' for word in sorted(words)) + "]"


def solve(words: list[str]) -> list[str]:
    seen: set[str] = set()
    ans: list[str] = []
    for word in sorted(words, key=lambda s: (len(s), s)):
        if can_form(word, seen):
            ans.append(word)
        seen.add(word)
    return ans


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx])
    idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        idx += 1
        words = data[idx:idx + n]
        idx += n
        out.append(fmt(solve(words)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
