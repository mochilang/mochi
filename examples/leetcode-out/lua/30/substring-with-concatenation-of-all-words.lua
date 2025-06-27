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
function __slice(obj, i, j)
    if i == nil then
        i = 0
    end
    if type(obj) == "string" then
        local len = #obj
        if j == nil then
            j = len
        end
        if i < 0 then
            i = len + i
        end
        if j < 0 then
            j = len + j
        end
        if i < 0 then
            i = 0
        end
        if j > len then
            j = len
        end
        return string.sub(obj, i + 1, j)
    elseif type(obj) == "table" then
        local len = #obj
        if j == nil then
            j = len
        end
        if i < 0 then
            i = len + i
        end
        if j < 0 then
            j = len + j
        end
        if i < 0 then
            i = 0
        end
        if j > len then
            j = len
        end
        local out = {}
        for k = i + 1, j do
            out[#out + 1] = obj[k]
        end
        return out
    else
        return {}
    end
end
function findSubstring(s, words)
    if __eq(#words, 0) then
        return {}
    end
    local wordLen = #__index(words, 0)
    local wordCount = #words
    local totalLen = (wordLen * wordCount)
    if (#s < totalLen) then
        return {}
    end
    local freq = {}
    for _, w in __iter(words) do
        if __contains(freq, w) then
            freq[w] = __add(__index(freq, w), 1)
        else
            freq[w] = 1
        end
        ::__continue0::
    end
    local result = {}
    for offset = 0, (wordLen) - 1 do
        local left = offset
        local count = 0
        local seen = {}
        local j = offset
        while (__add(j, wordLen) <= #s) do
            local word = __slice(s, j, __add(j, wordLen))
            j = __add(j, wordLen)
            if __contains(freq, word) then
                if __contains(seen, word) then
                    seen[word] = __add(__index(seen, word), 1)
                else
                    seen[word] = 1
                end
                count = __add(count, 1)
                while (__index(seen, word) > __index(freq, word)) do
                    local lw = __slice(s, left, __add(left, wordLen))
                    seen[lw] = (__index(seen, lw) - 1)
                    left = __add(left, wordLen)
                    count = (count - 1)
                    ::__continue3::
                end
                if __eq(count, wordCount) then
                    result = __add(result, {left})
                    local lw = __slice(s, left, __add(left, wordLen))
                    seen[lw] = (__index(seen, lw) - 1)
                    left = __add(left, wordLen)
                    count = (count - 1)
                end
            else
                seen = {}
                count = 0
                left = j
            end
            ::__continue2::
        end
        ::__continue1::
    end
    return result
end

function test_example_1()
    if not (__eq(findSubstring("barfoothefoobarman", {"foo", "bar"}), {0, 9})) then
        error("expect failed")
    end
end

function test_example_2()
    if not (__eq(findSubstring("wordgoodgoodgoodbestword", {"word", "good", "best", "word"}), {})) then
        error("expect failed")
    end
end

function test_example_3()
    if not (__eq(findSubstring("barfoofoobarthefoobarman", {"bar", "foo", "the"}), {6, 9, 12})) then
        error("expect failed")
    end
end

local __tests = {
    {name = "example 1", fn = test_example_1},
    {name = "example 2", fn = test_example_2},
    {name = "example 3", fn = test_example_3}
}
__run_tests(__tests)
