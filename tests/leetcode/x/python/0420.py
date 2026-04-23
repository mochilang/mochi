import sys

def strong_password_checker(s: str) -> int:
    n = len(s)
    missing = int(not any(c.islower() for c in s))
    missing += int(not any(c.isupper() for c in s))
    missing += int(not any(c.isdigit() for c in s))
    runs: list[int] = []
    i = 0
    while i < n:
        j = i
        while j < n and s[j] == s[i]:
            j += 1
        if j - i >= 3:
            runs.append(j - i)
        i = j
    if n < 6:
        return max(missing, 6 - n)
    if n <= 20:
        return max(missing, sum(r // 3 for r in runs))
    deletions = n - 20
    remain = deletions
    runs.sort(key=lambda r: r % 3)
    for idx, run in enumerate(runs):
        if remain <= 0:
            break
        need = run % 3 + 1
        use = min(remain, need)
        runs[idx] -= use
        remain -= use
    if remain > 0:
        for idx, run in enumerate(runs):
            if remain <= 0:
                break
            use = min(remain, max(0, run - 2))
            runs[idx] -= use
            remain -= use
    replacements = sum(max(0, r) // 3 for r in runs)
    return deletions + max(missing, replacements)

def main() -> None:
    data = sys.stdin.read().splitlines()
    if not data:
        return
    t = int(data[0].strip())
    ans = [str(strong_password_checker(data[i + 1].strip())) for i in range(t)]
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
