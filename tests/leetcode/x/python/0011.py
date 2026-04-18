import sys

def max_area(h):
    left, right = 0, len(h) - 1
    best = 0
    while left < right:
        height = h[left] if h[left] < h[right] else h[right]
        area = (right - left) * height
        if area > best:
            best = area
        if h[left] < h[right]:
            left += 1
        else:
            right -= 1
    return best

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0
        idx += 1
        arr = []
        for _ in range(n):
            arr.append(int(lines[idx].strip()) if idx < len(lines) else 0)
            idx += 1
        out.append(str(max_area(arr)))
    sys.stdout.write("\n".join(out))

if __name__ == '__main__':
    main()
