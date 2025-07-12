x = 8
msg = (function()
    if (x > 10) then
        return "big"
    else
        return (function()
    if (x > 5) then
        return "medium"
    else
        return "small"
    end
end)()
    end
end)()
print(msg)
