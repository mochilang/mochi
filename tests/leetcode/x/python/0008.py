import sys

INT_MAX = 2147483647
INT_MIN = -2147483648

def my_atoi(s: str) -> int:
    i = 0
    while i < len(s) and s[i] == ' ':
        i += 1
    sign = 1
    if i < len(s) and (s[i] == '+' or s[i] == '-'):
        if s[i] == '-':
            sign = -1
        i += 1
    ans = 0
    limit = 7 if sign > 0 else 8
    while i < len(s) and '0' <= s[i] <= '9':
        digit = ord(s[i]) - ord('0')
        if ans > 214748364 or (ans == 214748364 and digit > limit):
            return INT_MAX if sign > 0 else INT_MIN
        ans = ans * 10 + digit
        i += 1
    return sign * ans

def main() -> None:
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    t = int(lines[0].strip())
    out = [str(my_atoi(lines[i + 1] if i + 1 < len(lines) else '')) for i in range(t)]
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__':
    main()
