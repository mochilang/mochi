func boom() -> Bool {
    print("boom")
    return true
}

print((1 < 2) && (2 < 3) && (3 < 4))
print((1 < 2) && (2 > 3) && boom())
print((1 < 2) && (2 < 3) && (3 > 4) && boom())
