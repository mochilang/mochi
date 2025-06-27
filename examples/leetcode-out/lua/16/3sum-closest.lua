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
function threeSumClosest(nums, target)
    local sorted =
        (function()
        local _res = {}
        for _, n in ipairs(nums) do
            _res[#_res + 1] = {__key = n, __val = n}
        end
        local items = _res
        table.sort(
            items,
            function(a, b)
                return a.__key < b.__key
            end
        )
        local tmp = {}
        for _, it in ipairs(items) do
            tmp[#tmp + 1] = it.__val
        end
        items = tmp
        _res = items
        return _res
    end)()
    local n = #sorted
    local best = __add(__add(__index(sorted, 0), __index(sorted, 1)), __index(sorted, 2))
    for i = 0, (n) - 1 do
        local left = __add(i, 1)
        local right = (n - 1)
        while (left < right) do
            local sum = __add(__add(__index(sorted, i), __index(sorted, left)), __index(sorted, right))
            if __eq(sum, target) then
                return target
            end
            local diff = 0
            if (sum > target) then
                diff = (sum - target)
            else
                diff = (target - sum)
            end
            local bestDiff = 0
            if (best > target) then
                bestDiff = (best - target)
            else
                bestDiff = (target - best)
            end
            if (diff < bestDiff) then
                best = sum
            end
            if (sum < target) then
                left = __add(left, 1)
            else
                right = (right - 1)
            end
            ::__continue1::
        end
        ::__continue0::
    end
    return best
end

function test_example_1()
    if not (__eq(threeSumClosest({-1, 2, 1, -4}, 1), 2)) then
        error("expect failed")
    end
end

function test_example_2()
    if not (__eq(threeSumClosest({0, 0, 0}, 1), 0)) then
        error("expect failed")
    end
end

function test_additional()
    if not (__eq(threeSumClosest({1, 1, 1, 0}, -100), 2)) then
        error("expect failed")
    end
end

local __tests = {
    {name = "example 1", fn = test_example_1},
    {name = "example 2", fn = test_example_2},
    {name = "additional", fn = test_additional}
}
__run_tests(__tests)
