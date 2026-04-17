import sys


def expand(s: str, left: int, right: int) -> tuple[int, int]:
    while left >= 0 and right < len(s) and s[left] == s[right]:
        left -= 1
        right += 1
    return left + 1, right - left - 1


def longest_palindrome(s: str) -> str:
    best_start = 0
    best_len = 1 if s else 0
    for i in range(len(s)):
        start, length = expand(s, i, i)
        if length > best_len:
            best_start, best_len = start, length
        start, length = expand(s, i, i + 1)
        if length > best_len:
            best_start, best_len = start, length
    return s[best_start:best_start + best_len]


def main() -> None:
    data = sys.stdin.read().splitlines()
    if not data:
        return
    t = int(data[0].strip())
    lines = [longest_palindrome(data[i + 1] if i + 1 < len(data) else '') for i in range(t)]
    sys.stdout.write('\n'.join(lines))


if __name__ == '__main__':
    main()
