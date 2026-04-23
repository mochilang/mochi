import sys


def is_pal(s: str) -> bool:
    return s == s[::-1]


def palindrome_pairs(words: list[str]) -> list[list[int]]:
    pos = {w: i for i, w in enumerate(words)}
    ans: list[list[int]] = []
    for i, w in enumerate(words):
        for cut in range(len(w) + 1):
            pre, suf = w[:cut], w[cut:]
            if is_pal(pre):
                j = pos.get(suf[::-1])
                if j is not None and j != i:
                    ans.append([j, i])
            if cut < len(w) and is_pal(suf):
                j = pos.get(pre[::-1])
                if j is not None and j != i:
                    ans.append([i, j])
    ans.sort()
    return ans


def fmt(pairs: list[list[int]]) -> str:
    return "[" + ",".join(f"[{a},{b}]" for a, b in pairs) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        n = int(data[idx]); idx += 1
        words = [("" if data[idx + i] == "_" else data[idx + i]) for i in range(n)]
        idx += n
        if tc: out.append("")
        out.append(fmt(palindrome_pairs(words)))
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
