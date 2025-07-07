def boom():
    print("boom")
    return True

print(str((1 < 2) and (2 < 3) and (3 < 4)).lower())
print(str((1 < 2) and (2 > 3) and boom()).lower())
print(str((1 < 2) and (2 < 3) and (3 > 4) and boom()).lower())
