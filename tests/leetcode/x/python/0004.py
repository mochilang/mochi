import sys

def median(a, b):
    merged = []
    i = j = 0
    while i < len(a) and j < len(b):
        if a[i] <= b[j]:
            merged.append(a[i]); i += 1
        else:
            merged.append(b[j]); j += 1
    merged.extend(a[i:])
    merged.extend(b[j:])
    n = len(merged)
    if n % 2 == 1:
        return merged[n // 2]
    return (merged[n // 2 - 1] + merged[n // 2]) / 2.0

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        a = []
        for _ in range(n): a.append(int(lines[idx].strip()) if idx < len(lines) else 0); idx += 1
        m = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        b = []
        for _ in range(m): b.append(int(lines[idx].strip()) if idx < len(lines) else 0); idx += 1
        out.append(f"{median(a,b):.1f}")
    sys.stdout.write("\n".join(out))

if __name__ == '__main__':
    main()
