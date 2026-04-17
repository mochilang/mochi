import sys


def lcp(strs):
    prefix = strs[0]
    while not all(s.startswith(prefix) for s in strs):
        prefix = prefix[:-1]
    return prefix


def main() -> None:
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    idx = 0
    t = int(tokens[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(tokens[idx]); idx += 1
        strs = tokens[idx:idx + n]; idx += n
        out.append(f'"{lcp(strs)}"')
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
