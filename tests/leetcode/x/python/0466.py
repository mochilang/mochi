import sys

def get_max_repetitions(s1: str, n1: int, s2: str, n2: int) -> int:
    if set(s2) - set(s1):
        return 0
    seen: dict[int, tuple[int, int]] = {}
    s1_count = 0
    s2_count = 0
    idx2 = 0
    while True:
        s1_count += 1
        for ch in s1:
            if ch == s2[idx2]:
                idx2 += 1
                if idx2 == len(s2):
                    s2_count += 1
                    idx2 = 0
        if s1_count == n1:
            return s2_count // n2
        if idx2 in seen:
            prev_s1, prev_s2 = seen[idx2]
            prefix_s1, prefix_s2 = prev_s1, prev_s2
            loop_s1 = s1_count - prev_s1
            loop_s2 = s2_count - prev_s2
            break
        seen[idx2] = (s1_count, s2_count)
    ans = prefix_s2
    remain = n1 - prefix_s1
    ans += (remain // loop_s1) * loop_s2
    rest = remain % loop_s1
    for _ in range(rest):
        for ch in s1:
            if ch == s2[idx2]:
                idx2 += 1
                if idx2 == len(s2):
                    ans += 1
                    idx2 = 0
    return ans // n2

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        s1 = data[idx]; n1 = int(data[idx + 1]); s2 = data[idx + 2]; n2 = int(data[idx + 3]); idx += 4
        ans.append(str(get_max_repetitions(s1, n1, s2, n2)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
