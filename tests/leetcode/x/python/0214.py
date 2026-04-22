import sys


def solve(s: str) -> str:
    rev = s[::-1]
    combined = s + "#" + rev
    pi = [0] * len(combined)
    for i in range(1, len(combined)):
        j = pi[i - 1]
        while j > 0 and combined[i] != combined[j]:
            j = pi[j - 1]
        if combined[i] == combined[j]:
            j += 1
        pi[i] = j
    keep = pi[-1] if pi else 0
    return rev[: len(s) - keep] + s


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = []
    for i in range(t):
        out.append(solve(lines[i + 1] if i + 1 < len(lines) else ""))
    sys.stdout.write("\n".join(out))
