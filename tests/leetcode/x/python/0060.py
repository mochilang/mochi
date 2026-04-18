import sys

def get_permutation(n, k):
    digits = [str(i) for i in range(1, n + 1)]
    fact = [1] * (n + 1)
    for i in range(1, n + 1):
        fact[i] = fact[i - 1] * i
    k -= 1
    out = []
    for rem in range(n, 0, -1):
        block = fact[rem - 1]
        idx = k // block
        k %= block
        out.append(digits.pop(idx))
    return ''.join(out)

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    idx = 0
    t = int(lines[idx].strip())
    idx += 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip())
        idx += 1
        k = int(lines[idx].strip())
        idx += 1
        out.append(get_permutation(n, k))
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__':
    main()
