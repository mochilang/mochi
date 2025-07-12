nums = {1, 2, 3}
letters = {"A", "B"}
pairs = (function()
    local _res = {}
    for _, n in ipairs(nums) do
        for _, l in ipairs(letters) do
            if ((n % 2) == 0) then
                _res[#_res+1] = {["n"]=n, ["l"]=l}
            end
        end
    end
    return _res
end)()
print("--- Even pairs ---")
for _, p in ipairs(pairs) do
    print(p.n, p.l)
    ::__continue0::
end
