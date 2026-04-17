import sys


def add_lists(a, b):
    i = j = carry = 0
    out = []
    while i < len(a) or j < len(b) or carry:
        total = carry
        if i < len(a):
            total += a[i]
            i += 1
        if j < len(b):
            total += b[j]
            j += 1
        out.append(total % 10)
        carry = total // 10
    return out


def fmt(arr):
    return '[' + ','.join(str(x) for x in arr) + ']'


def main():
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    idx = 0
    t = int(tokens[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(tokens[idx]); idx += 1
        a = list(map(int, tokens[idx:idx+n])); idx += n
        m = int(tokens[idx]); idx += 1
        b = list(map(int, tokens[idx:idx+m])); idx += m
        out.append(fmt(add_lists(a, b)))
    sys.stdout.write('\n'.join(out))


if __name__ == '__main__':
    main()
