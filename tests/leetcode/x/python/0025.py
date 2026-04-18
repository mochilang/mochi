import sys

def rev_groups(arr, k):
    out = arr[:]
    i = 0
    while i + k <= len(out):
        out[i:i+k] = reversed(out[i:i+k])
        i += k
    return out

def fmt(arr):
    return '[' + ','.join(str(x) for x in arr) + ']'

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    idx = 0
    t = int(lines[idx].strip()); idx += 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        arr = []
        for _ in range(n):
            arr.append(int(lines[idx].strip()) if idx < len(lines) else 0); idx += 1
        k = int(lines[idx].strip()) if idx < len(lines) else 1; idx += 1
        out.append(fmt(rev_groups(arr, k)))
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__':
    main()
