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
print("--- Adults ---")
for _, person in ipairs(adults) do
    print(person.name, "is", person.age, (function()
    if person.is_senior then
        return " (senior)"
    else
        return ""
    end
end)())
end
