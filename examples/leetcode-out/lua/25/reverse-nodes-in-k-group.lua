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
function reverseKGroup(nums, k)
    local n = #nums
    if (k <= 1) then
        return nums
    end
    local result = {}
    local i = 0
    while (i < n) do
        local _end = __add(i, k)
        if (_end <= n) then
            local j = (_end - 1)
            while (j >= i) do
                result = __add(result, {__index(nums, j)})
                j = (j - 1)
                ::__continue1::
            end
        else
            local j = i
            while (j < n) do
                result = __add(result, {__index(nums, j)})
                j = __add(j, 1)
                ::__continue2::
            end
        end
        i = __add(i, k)
        ::__continue0::
    end
    return result
end

function test_example_1()
    if not (__eq(reverseKGroup({1, 2, 3, 4, 5}, 2), {2, 1, 4, 3, 5})) then
        error("expect failed")
    end
end

function test_example_2()
    if not (__eq(reverseKGroup({1, 2, 3, 4, 5}, 3), {3, 2, 1, 4, 5})) then
        error("expect failed")
    end
end

function test_k_equals_list_length()
    if not (__eq(reverseKGroup({1, 2, 3, 4}, 4), {4, 3, 2, 1})) then
        error("expect failed")
    end
end

function test_k_greater_than_length()
    if not (__eq(reverseKGroup({1, 2, 3}, 5), {1, 2, 3})) then
        error("expect failed")
    end
end

function test_k_is_one()
    if not (__eq(reverseKGroup({1, 2, 3}, 1), {1, 2, 3})) then
        error("expect failed")
    end
end

local __tests = {
    {name = "example 1", fn = test_example_1},
    {name = "example 2", fn = test_example_2},
    {name = "k equals list length", fn = test_k_equals_list_length},
    {name = "k greater than length", fn = test_k_greater_than_length},
    {name = "k is one", fn = test_k_is_one}
}
__run_tests(__tests)
