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
function divide(dividend, divisor)
    if (__eq(dividend, (-2147483647 - 1)) and __eq(divisor, (-1))) then
        return 2147483647
    end
    local negative = false
    if (dividend < 0) then
        negative = not negative
        dividend = -dividend
    end
    if (divisor < 0) then
        negative = not negative
        divisor = -divisor
    end
    local quotient = 0
    while (dividend >= divisor) do
        local temp = divisor
        local multiple = 1
        while (dividend >= __add(temp, temp)) do
            temp = __add(temp, temp)
            multiple = __add(multiple, multiple)
            ::__continue1::
        end
        dividend = (dividend - temp)
        quotient = __add(quotient, multiple)
        ::__continue0::
    end
    if negative then
        quotient = -quotient
    end
    if (quotient > 2147483647) then
        return 2147483647
    end
    if (quotient < (-2147483647 - 1)) then
        return -2147483648
    end
    return quotient
end

function test_example_1()
    if not (__eq(divide(10, 3), 3)) then
        error("expect failed")
    end
end

function test_example_2()
    if not (__eq(divide(7, -3), (-2))) then
        error("expect failed")
    end
end

function test_overflow()
    if not (__eq(divide(-2147483648, -1), 2147483647)) then
        error("expect failed")
    end
end

function test_divide_by_1()
    if not (__eq(divide(12345, 1), 12345)) then
        error("expect failed")
    end
end

function test_negative_result()
    if not (__eq(divide(-15, 2), (-7))) then
        error("expect failed")
    end
end

local __tests = {
    {name = "example 1", fn = test_example_1},
    {name = "example 2", fn = test_example_2},
    {name = "overflow", fn = test_overflow},
    {name = "divide by 1", fn = test_divide_by_1},
    {name = "negative result", fn = test_negative_result}
}
__run_tests(__tests)
