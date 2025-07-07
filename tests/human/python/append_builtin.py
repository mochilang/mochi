# Translation of append_builtin.mochi to Python

a = [1, 2]

# Mochi 'append' returns a new list with the item appended
# We'll mimic this behaviour with list concatenation

def append(lst, item):
    return lst + [item]

print(append(a, 3))
