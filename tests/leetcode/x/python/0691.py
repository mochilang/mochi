import sys
from functools import lru_cache


def solve(stickers: list[str], target: str) -> int:
    sticker_counts: list[list[int]] = []
    for sticker in stickers:
        counts = [0] * 26
        for ch in sticker:
            counts[ord(ch) - ord("a")] += 1
        sticker_counts.append(counts)

    target_idx = [ord(ch) - ord("a") for ch in target]
    full_mask = (1 << len(target)) - 1

    @lru_cache(maxsize=None)
    def dfs(mask: int) -> int:
        if mask == full_mask:
            return 0
        first = 0
        while (mask >> first) & 1:
            first += 1
        need = target_idx[first]
        best = 10**9
        for counts in sticker_counts:
            if counts[need] == 0:
                continue
            remaining = counts[:]
            next_mask = mask
            for i, ch in enumerate(target_idx):
                if ((next_mask >> i) & 1) == 0 and remaining[ch] > 0:
                    remaining[ch] -= 1
                    next_mask |= 1 << i
            if next_mask != mask:
                best = min(best, 1 + dfs(next_mask))
        return best

    answer = dfs(0)
    return -1 if answer >= 10**9 else answer


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        idx += 1
        stickers = data[idx : idx + n]
        idx += n
        target = data[idx]
        idx += 1
        out.append(str(solve(stickers, target)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
