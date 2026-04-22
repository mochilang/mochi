import sys


def calculate(expr):
    result = 0
    number = 0
    sign = 1
    stack = []
    for ch in expr:
        if ch.isdigit():
            number = number * 10 + ord(ch) - ord("0")
        elif ch == "+" or ch == "-":
            result += sign * number
            number = 0
            sign = 1 if ch == "+" else -1
        elif ch == "(":
            stack.append(result)
            stack.append(sign)
            result = 0
            number = 0
            sign = 1
        elif ch == ")":
            result += sign * number
            number = 0
            prev_sign = stack.pop()
            prev_result = stack.pop()
            result = prev_result + prev_sign * result
    return result + sign * number


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = [str(calculate(lines[i + 1])) for i in range(t)]
    sys.stdout.write("\n".join(out))
