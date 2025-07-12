m = {["a"]=1, ["b"]=2}
local _m0 = m
local _k0 = {}
for k in pairs(_m0) do _k0[#_k0+1] = k end
table.sort(_k0, function(a,b) return tostring(a)<tostring(b) end)
for _, k in ipairs(_k0) do
    print(k)
end
