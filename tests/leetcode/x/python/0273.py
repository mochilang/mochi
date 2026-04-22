import sys

LESS20 = [
    "",
    "One",
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
    "Ten",
    "Eleven",
    "Twelve",
    "Thirteen",
    "Fourteen",
    "Fifteen",
    "Sixteen",
    "Seventeen",
    "Eighteen",
    "Nineteen",
]
TENS = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
THOUSANDS = ["", "Thousand", "Million", "Billion"]


def helper(n):
    if n == 0:
        return ""
    if n < 20:
        return LESS20[n]
    if n < 100:
        return TENS[n // 10] + ("" if n % 10 == 0 else " " + helper(n % 10))
    return LESS20[n // 100] + " Hundred" + ("" if n % 100 == 0 else " " + helper(n % 100))


def solve(num):
    if num == 0:
        return "Zero"
    parts = []
    idx = 0
    while num > 0:
        chunk = num % 1000
        if chunk:
            words = helper(chunk)
            if THOUSANDS[idx]:
                words += " " + THOUSANDS[idx]
            parts.append(words)
        num //= 1000
        idx += 1
    return " ".join(reversed(parts))


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = [solve(int(lines[i + 1].strip())) for i in range(t)]
    sys.stdout.write("\n".join(out))
