import sys
from collections import defaultdict

def min_transfers(transactions: list[list[int]]) -> int:
    balance: dict[int, int] = defaultdict(int)
    for a, b, x in transactions:
        balance[a] -= x
        balance[b] += x
    debts = [v for v in balance.values() if v]
    def dfs(i: int) -> int:
        while i < len(debts) and debts[i] == 0:
            i += 1
        if i == len(debts):
            return 0
        best = 10**9
        seen: set[int] = set()
        for j in range(i + 1, len(debts)):
            if debts[i] * debts[j] < 0 and debts[j] not in seen:
                seen.add(debts[j])
                debts[j] += debts[i]
                best = min(best, 1 + dfs(i + 1))
                debts[j] -= debts[i]
                if debts[j] + debts[i] == 0:
                    break
        return best
    return dfs(0)

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        m = int(data[idx]); idx += 1
        trans = []
        for _ in range(m):
            trans.append([int(data[idx]), int(data[idx + 1]), int(data[idx + 2])])
            idx += 3
        ans.append(str(min_transfers(trans)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
