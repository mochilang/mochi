import sys

def trap(height):
    left = 0
    right = len(height) - 1
    left_max = 0
    right_max = 0
    water = 0
    while left <= right:
        if left_max <= right_max:
            if height[left] < left_max:
                water += left_max - height[left]
            else:
                left_max = height[left]
            left += 1
        else:
            if height[right] < right_max:
                water += right_max - height[right]
            else:
                right_max = height[right]
            right -= 1
    return water

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
        out.append(str(trap(arr)))
    sys.stdout.write("\n".join(out))

if __name__ == '__main__':
    main()
