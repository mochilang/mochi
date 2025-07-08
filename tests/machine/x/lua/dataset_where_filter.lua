function __print(...)
    local args = {...}
    local function to_str(v)
        if v == nil then return '<nil>' end
        if type(v) == 'table' then
            if v[1] ~= nil or #v > 0 then
                local parts = {}
                for i,x in ipairs(v) do parts[#parts+1] = tostring(x) end
                return table.concat(parts, ' ')
            end
        end
        if type(v) == 'number' and v == math.floor(v) then
            return tostring(math.floor(v))
        end
        return tostring(v)
    end
    for i, a in ipairs(args) do
        if i > 1 then io.write(' ') end
        io.write(to_str(a))
    end
    io.write('\n')
end
people = {{["name"]="Alice", ["age"]=30}, {["name"]="Bob", ["age"]=15}, {["name"]="Charlie", ["age"]=65}, {["name"]="Diana", ["age"]=45}}
adults = (function()
    local _res = {}
    for _, person in ipairs(people) do
        if (person.age >= 18) then
            _res[#_res+1] = {["name"]=person.name, ["age"]=person.age, ["is_senior"]=(person.age >= 60)}
        end
    end
    return _res
end)()
__print("--- Adults ---")
for _, person in ipairs(adults) do
    __print(person.name, "is", person.age, (function()
    if person.is_senior then
        return " (senior)"
    else
        return ""
    end
end)())
    ::__continue0::
end
