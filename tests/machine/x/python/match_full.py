x = 2
if x == 1:
    label = "one"
elif x == 2:
    label = "two"
elif x == 3:
    label = "three"
else:
    label = "unknown"
print(label)

day = "sun"
if day == "mon":
    mood = "tired"
elif day == "fri":
    mood = "excited"
elif day == "sun":
    mood = "relaxed"
else:
    mood = "normal"
print(mood)

ok = True
status = "confirmed" if ok else "denied"
print(status)

def classify(n: int) -> str:
    if n == 0:
        return "zero"
    elif n == 1:
        return "one"
    else:
        return "many"

print(classify(0))
print(classify(5))
