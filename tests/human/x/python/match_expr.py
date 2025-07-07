x = 2
match x:
    case 1:
        label = "one"
    case 2:
        label = "two"
    case 3:
        label = "three"
    case _:
        label = "unknown"
print(label)
