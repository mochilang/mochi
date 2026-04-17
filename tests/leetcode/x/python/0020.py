import sys


def is_valid(s: str) -> bool:
    stack = []
    for ch in s:
        if ch in '([{':
            stack.append(ch)
        else:
            if not stack:
                return False
            open_br = stack.pop()
            if (ch == ')' and open_br != '(') or (ch == ']' and open_br != '[') or (ch == '}' and open_br != '{'):
                return False
    return not stack


def main() -> None:
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    t = int(tokens[0])
    out = ['true' if is_valid(s) else 'false' for s in tokens[1:1 + t]]
    sys.stdout.write('\n'.join(out))


if __name__ == '__main__':
    main()
