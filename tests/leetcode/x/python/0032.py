import sys

def solve_case(s):
    stack = [-1]
    best = 0
    for i, ch in enumerate(s):
        if ch == '(':
            stack.append(i)
        else:
            stack.pop()
            if not stack:
                stack.append(i)
            else:
                best = max(best, i - stack[-1])
    return best

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    idx = 0; t = int(lines[idx].strip()); idx += 1; out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0; idx += 1
        s = lines[idx] if n > 0 and idx < len(lines) else ''
        if n > 0: idx += 1
        out.append(str(solve_case(s)))
    sys.stdout.write('\n'.join(out))
if __name__ == '__main__': main()
