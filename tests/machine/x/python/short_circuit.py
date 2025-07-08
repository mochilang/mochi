def boom(a, b):
    print("boom")
    return True
print(False and boom(1, 2))
print(True or boom(1, 2))
