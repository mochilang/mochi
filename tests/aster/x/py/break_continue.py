# Code generated by Mochi transpiler.
# Version 0.10.36, generated on 2025-07-22 17:46 +0700
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
for n in numbers:
    if n % 2 == 0:
        continue
    if n > 7:
        break
    print("odd number:", n)
