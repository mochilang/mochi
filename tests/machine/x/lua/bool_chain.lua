function boom()
    print("boom")
    return true
end

print(((((1 < 2)) and ((2 < 3))) and ((3 < 4))))
print(((((1 < 2)) and ((2 > 3))) and boom()))
print((((((1 < 2)) and ((2 < 3))) and ((3 > 4))) and boom()))
