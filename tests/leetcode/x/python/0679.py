import sys

EPS = 1e-6


def solve(cards: list[int]) -> bool:
    nums = [float(x) for x in cards]

    def dfs(arr: list[float]) -> bool:
        if len(arr) == 1:
            return abs(arr[0] - 24.0) < EPS
        n = len(arr)
        for i in range(n):
            for j in range(i + 1, n):
                rest = [arr[k] for k in range(n) if k != i and k != j]
                a, b = arr[i], arr[j]
                candidates = [a + b, a * b, a - b, b - a]
                if abs(b) > EPS:
                    candidates.append(a / b)
                if abs(a) > EPS:
                    candidates.append(b / a)
                for value in candidates:
                    if dfs(rest + [value]):
                        return True
        return False

    return dfs(nums)


def main() -> None:
    lines = [line.strip() for line in sys.stdin.read().splitlines() if line.strip()]
    if not lines:
        return
    out = ["true" if solve(list(map(int, line.split()))) else "false" for line in lines[1:1 + int(lines[0])]]
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
