function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
end
print((6 * 7))
print(__div(7, 2))
print((7 % 2))
