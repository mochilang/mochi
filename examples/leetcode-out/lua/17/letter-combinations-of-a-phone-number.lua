function __add(a, b)
    if type(a) == "table" and type(b) == "table" then
        local out = {}
        for i = 1, #a do
            out[#out + 1] = a[i]
        end
        for i = 1, #b do
            out[#out + 1] = b[i]
        end
        return out
    elseif type(a) == "string" or type(b) == "string" then
        return tostring(a) .. tostring(b)
    else
        return a + b
    end
end
function __contains(container, item)
    if type(container) == "table" then
        if container[1] ~= nil or #container > 0 then
            for _, v in ipairs(container) do
                if v == item then
                    return true
                end
            end
            return false
        else
            return container[item] ~= nil
        end
    elseif type(container) == "string" then
        return string.find(container, item, 1, true) ~= nil
    else
        return false
    end
end
function __eq(a, b)
    if type(a) ~= type(b) then
        return false
    end
    if type(a) == "number" then
        return math.abs(a - b) < 1e-9
    end
    if type(a) ~= "table" then
        return a == b
    end
    if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then
        if #a ~= #b then
            return false
        end
        for i = 1, #a do
            if not __eq(a[i], b[i]) then
                return false
            end
        end
        return true
    end
    for k, v in pairs(a) do
        if not __eq(v, b[k]) then
            return false
        end
    end
    for k, _ in pairs(b) do
        if a[k] == nil then
            return false
        end
    end
    return true
end
function __index(obj, i)
    if type(obj) == "string" then
        return __indexString(obj, i)
    elseif type(obj) == "table" then
        if obj[1] ~= nil or #obj > 0 then
            return obj[(i) + 1]
        else
            return obj[i]
        end
    else
        error("cannot index")
    end
end
function __indexString(s, i)
    local len = #s
    if i < 0 then
        i = len + i + 1
    else
        i = i + 1
    end
    if i < 1 or i > len then
        error("index out of range")
    end
    return string.sub(s, i, i)
end
function __iter(obj)
    if type(obj) == "table" then
        if obj[1] ~= nil or #obj > 0 then
            local i = 0
            local n = #obj
            return function()
                i = i + 1
                if i <= n then
                    return i, obj[i]
                end
            end
        else
            return pairs(obj)
        end
    elseif type(obj) == "string" then
        local i = 0
        local n = #obj
        return function()
            i = i + 1
            if i <= n then
                return i, string.sub(obj, i, i)
            end
        end
    else
        return function()
            return nil
        end
    end
end
function __run_tests(tests)
    local function format_duration(d)
        if d < 1e-6 then
            return string.format("%dns", math.floor(d * 1e9))
        end
        if d < 1e-3 then
            return string.format("%.1fµs", d * 1e6)
        end
        if d < 1 then
            return string.format("%.1fms", d * 1e3)
        end
        return string.format("%.2fs", d)
    end
    local failures = 0
    for _, t in ipairs(tests) do
        io.write("   test " .. t.name .. " ...")
        local start = os.clock()
        local ok, err = pcall(t.fn)
        local dur = os.clock() - start
        if ok then
            io.write(" ok (" .. format_duration(dur) .. ")\n")
        else
            io.write(" fail " .. tostring(err) .. " (" .. format_duration(dur) .. ")\n")
            failures = failures + 1
        end
    end
    if failures > 0 then
        io.write("\n[FAIL] " .. failures .. " test(s) failed.\n")
    end
end
function letterCombinations(digits)
    if __eq(#digits, 0) then
        return {}
    end
    local mapping = {
        ["2"] = {"a", "b", "c"},
        ["3"] = {"d", "e", "f"},
        ["4"] = {"g", "h", "i"},
        ["5"] = {"j", "k", "l"},
        ["6"] = {"m", "n", "o"},
        ["7"] = {"p", "q", "r", "s"},
        ["8"] = {"t", "u", "v"},
        ["9"] = {"w", "x", "y", "z"}
    }
    local result = {""}
    for _, d in __iter(digits) do
        if not (__contains(mapping, d)) then
            goto __continue0
        end
        local letters = __index(mapping, d)
        local next = (function()
            local _res = {}
            for _, p in ipairs(result) do
                for _, ch in ipairs(letters) do
                    _res[#_res + 1] = __add(p, ch)
                end
            end
            return _res
        end)()
        result = next
        ::__continue0::
    end
    return result
end

function test_example_1()
    if not (__eq(letterCombinations("23"), {"ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"})) then
        error("expect failed")
    end
end

function test_example_2()
    if not (__eq(letterCombinations(""), {})) then
        error("expect failed")
    end
end

function test_example_3()
    if not (__eq(letterCombinations("2"), {"a", "b", "c"})) then
        error("expect failed")
    end
end

function test_single_seven()
    if not (__eq(letterCombinations("7"), {"p", "q", "r", "s"})) then
        error("expect failed")
    end
end

function test_mix()
    if
        not (__eq(
            letterCombinations("79"),
            {"pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy", "sz"}
        ))
     then
        error("expect failed")
    end
end

local __tests = {
    {name = "example 1", fn = test_example_1},
    {name = "example 2", fn = test_example_2},
    {name = "example 3", fn = test_example_3},
    {name = "single seven", fn = test_single_seven},
    {name = "mix", fn = test_mix}
}
__run_tests(__tests)
