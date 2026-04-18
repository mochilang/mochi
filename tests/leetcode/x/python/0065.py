import sys
def is_number(s):
    seen_digit = seen_dot = seen_exp = False
    digit_after_exp = True
    for i, ch in enumerate(s):
        if '0' <= ch <= '9':
            seen_digit = True
            if seen_exp:
                digit_after_exp = True
        elif ch in '+-':
            if i != 0 and s[i - 1] not in 'eE':
                return False
        elif ch == '.':
            if seen_dot or seen_exp:
                return False
            seen_dot = True
        elif ch in 'eE':
            if seen_exp or not seen_digit:
                return False
            seen_exp = True
            digit_after_exp = False
        else:
            return False
    return seen_digit and digit_after_exp
lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    print('\n'.join('true' if is_number(lines[i + 1]) else 'false' for i in range(t)), end='')
