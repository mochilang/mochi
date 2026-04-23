import sys


def rearrange(s: str, k: int) -> str:
    if k <= 1:
        return s
    counts = [0] * 26
    last = [-10**9] * 26
    for ch in s:
        counts[ord(ch) - 97] += 1
    out: list[str] = []
    for pos in range(len(s)):
        best = -1
        for i in range(26):
            if counts[i] > 0 and pos - last[i] >= k:
                if best < 0 or counts[i] > counts[best]:
                    best = i
        if best < 0:
            return ""
        out.append(chr(97 + best))
        counts[best] -= 1
        last[best] = pos
    return "".join(out)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        s = data[idx]; k = int(data[idx + 1]); idx += 2
        if tc: out.append("")
        out.append("\"" + rearrange(s, k) + "\"")
    sys.stdout.write("\n".join(out))

if __name__ == "__main__": main()
