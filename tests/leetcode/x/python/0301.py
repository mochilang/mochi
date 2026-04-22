import sys


def solve(s: str) -> list[str]:
    left_remove = 0
    right_remove = 0
    for ch in s:
        if ch == "(":
            left_remove += 1
        elif ch == ")":
            if left_remove > 0:
                left_remove -= 1
            else:
                right_remove += 1

    ans: set[str] = set()

    def dfs(i: int, left: int, right: int, balance: int, path: list[str]) -> None:
        if i == len(s):
            if left == 0 and right == 0 and balance == 0:
                ans.add("".join(path))
            return

        ch = s[i]
        if ch == "(":
            if left > 0:
                dfs(i + 1, left - 1, right, balance, path)
            path.append(ch)
            dfs(i + 1, left, right, balance + 1, path)
            path.pop()
        elif ch == ")":
            if right > 0:
                dfs(i + 1, left, right - 1, balance, path)
            if balance > 0:
                path.append(ch)
                dfs(i + 1, left, right, balance - 1, path)
                path.pop()
        else:
            path.append(ch)
            dfs(i + 1, left, right, balance, path)
            path.pop()

    dfs(0, left_remove, right_remove, 0, [])
    return sorted(ans)


def main() -> None:
    lines = [line.rstrip("\n") for line in sys.stdin]
    if not lines:
        return
    t = int(lines[0].strip())
    out: list[str] = []
    for tc in range(t):
        ans = solve(lines[tc + 1])
        if tc:
            out.append("")
        out.append(str(len(ans)))
        out.extend(ans)
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
