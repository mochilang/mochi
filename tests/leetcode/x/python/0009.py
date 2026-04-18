import sys


def is_palindrome(x: int) -> bool:
    if x < 0:
        return False
    original = x
    rev = 0
    while x > 0:
        rev = rev * 10 + x % 10
        x //= 10
    return rev == original


def main() -> None:
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    t = int(tokens[0])
    out = ["true" if is_palindrome(int(x)) else "false" for x in tokens[1:1 + t]]
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
