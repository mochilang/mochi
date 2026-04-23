import sys

def abbr_len(mask: int, n: int) -> int:
    total = 0
    skipped = False
    for i in range(n):
        if (mask >> i) & 1:
            if skipped:
                total += 1
                skipped = False
            total += 1
        else:
            skipped = True
    return total + (1 if skipped else 0)

def build_abbr(target: str, mask: int) -> str:
    out: list[str] = []
    run = 0
    for i, ch in enumerate(target):
        if (mask >> i) & 1:
            if run:
                out.append(str(run))
                run = 0
            out.append(ch)
        else:
            run += 1
    if run:
        out.append(str(run))
    return ''.join(out)

def min_abbreviation(target: str, dictionary: list[str]) -> str:
    n = len(target)
    diffs: list[int] = []
    for word in dictionary:
        if len(word) != n:
            continue
        diff = 0
        for i, (a, b) in enumerate(zip(target, word)):
            if a != b:
                diff |= 1 << i
        diffs.append(diff)
    if not diffs:
        return str(n)
    best_mask = 0
    best_len = n + 1
    for mask in range(1 << n):
        if all(mask & diff for diff in diffs):
            length = abbr_len(mask, n)
            if length < best_len:
                best_len = length
                best_mask = mask
    return build_abbr(target, best_mask)

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        target = data[idx]; idx += 1
        m = int(data[idx]); idx += 1
        words = data[idx:idx+m]; idx += m
        ans.append(min_abbreviation(target, words))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
