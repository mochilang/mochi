x = 2
label = ("one" if x == 1 else ("two" if x == 2 else ("three" if x == 3 else "unknown")))
print(label)
day = "sun"
mood = ("tired" if day == "mon" else ("excited" if day == "fri" else ("relaxed" if day == "sun" else "normal")))
print(mood)
ok = True
status = ("confirmed" if ok == True else ("denied" if ok == False else None))
print(status)
def classify(n):
    return ("zero" if n == 0 else ("one" if n == 1 else "many"))
print(classify(0))
print(classify(5))
