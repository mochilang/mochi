def boom(a, b):
    print("boom")
    return True


print(str(False and boom(1, 2)).lower())
print(str(True or boom(1, 2)).lower())
