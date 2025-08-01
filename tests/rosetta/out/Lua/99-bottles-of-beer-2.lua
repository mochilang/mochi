-- Generated by Mochi compiler v0.10.28 on 2025-07-18T11:14:51Z
function __add(a, b)
    if type(a) == 'table' and type(b) == 'table' then
        local out = {}
        for i = 1, #a do out[#out+1] = a[i] end
        for i = 1, #b do out[#out+1] = b[i] end
        return out
    elseif type(a) == 'string' or type(b) == 'string' then
        return tostring(a) .. tostring(b)
    else
        return a + b
    end
end
function __append(lst, v)
    local out = {}
    if lst then for i = 1, #lst do out[#out+1] = lst[i] end end
    out[#out+1] = v
    return out
end
function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
end
function __index(obj, i)
    if type(obj) == 'string' then
        return __indexString(obj, i)
    elseif type(obj) == 'table' then
        if obj[1] ~= nil or #obj > 0 then
            return obj[(i)+1]
        else
            return obj[i]
        end
    else
        error('cannot index')
    end
end
function __indexString(s, i)
    local len = #s
    if i < 0 then
        i = len + i + 1
    else
        i = i + 1
    end
    if i < 1 or i > len then error('index out of range') end
    return string.sub(s, i, i)
end
function __slice(obj, i, j)
    if i == nil then i = 0 end
    if type(obj) == 'string' then
        local len = #obj
        if j == nil then j = len end
        if i < 0 then i = len + i end
        if j < 0 then j = len + j end
        if i < 0 then i = 0 end
        if j > len then j = len end
        return string.sub(obj, i+1, j)
    elseif type(obj) == 'table' then
        local len = #obj
        if j == nil then j = len end
        if i < 0 then i = len + i end
        if j < 0 then j = len + j end
        if i < 0 then i = 0 end
        if j > len then j = len end
        local out = {}
        for k = i+1, j do
            out[#out+1] = obj[k]
        end
        return out
    else
        return {}
    end
end
function fields(s)
    local words = {};
    local cur = "";
    local i = 0;
    while (i < #s) do
        local ch = __slice(s, i, __add(i, 1));
        if (((ch == " ") or (ch == "\n")) or (ch == "\t")) then
            if (#cur > 0) then
                words = __append(words, cur);
                cur = "";
            end
        else
            cur = __add(cur, ch);
        end
        i = __add(i, 1);
    end
    if (#cur > 0) then
        words = __append(words, cur);
    end
    return words
end

function join(xs, sep)
    local res = "";
    local i = 0;
    while (i < #xs) do
        if (i > 0) then
            res = __add(res, sep);
        end
        res = __add(res, __index(xs, i));
        i = __add(i, 1);
    end
    return res
end

function numberName(n)
    local small = {"no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};
    local tens = {"ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};
    if (n < 0) then
        return ""
    end
    if (n < 20) then
        return __index(small, n)
    end
    if (n < 100) then
        local t = __index(tens, tonumber((__div(n, 10))));
        local s = (n % 10);
        if (s > 0) then
            t = __add(__add(t, " "), __index(small, s));
        end
        return t
    end
    return ""
end

function pluralizeFirst(s, n)
    if (n == 1) then
        return s
    end
    local w = fields(s);
    if (#w > 0) then
        w[0] = __add(__index(w, 0), "s");
    end
    return join(w, " ")
end

function randInt(seed, n)
    local next = ((__add((seed * 1664525), 1013904223)) % 2147483647);
    return (next % n)
end

function slur(p, d)
    if (#p <= 2) then
        return p
    end
    local a = {};
    local i = 1;
    while (i < (#p - 1)) do
        a = __append(a, __slice(p, i, __add(i, 1)));
        i = __add(i, 1);
    end
    local idx = (#a - 1);
    local seed = d;
    while (idx >= 1) do
        seed = ((__add((seed * 1664525), 1013904223)) % 2147483647);
        if ((seed % 100) >= d) then
            local j = (seed % (__add(idx, 1)));
            local tmp = __index(a, idx);
            a[idx] = __index(a, j);
            a[j] = tmp;
        end
        idx = (idx - 1);
    end
    local s = __slice(p, 0, 1);
    local k = 0;
    while (k < #a) do
        s = __add(s, __index(a, k));
        k = __add(k, 1);
    end
    s = __add(s, __slice(p, (#p - 1), #p));
    local w = fields(s);
    return join(w, " ")
end

function main()
    local i = 99;
    while (i > 0) do
        print(((((slur(numberName(i), i) .. " ") .. pluralizeFirst(slur("bottle of", i), i)) .. " ") .. slur("beer on the wall", i)));
        print(((((slur(numberName(i), i) .. " ") .. pluralizeFirst(slur("bottle of", i), i)) .. " ") .. slur("beer", i)));
        print(((((slur("take one", i) .. " ") .. slur("down", i)) .. " ") .. slur("pass it around", i)));
        print(((((slur(numberName((i - 1)), i) .. " ") .. pluralizeFirst(slur("bottle of", i), (i - 1))) .. " ") .. slur("beer on the wall", i)));
        i = (i - 1);
    end
end

main();
