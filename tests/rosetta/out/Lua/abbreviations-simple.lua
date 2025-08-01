-- Generated by Mochi compiler v0.10.28 on 2025-07-18T11:14:38Z
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

function padRight(s, width)
    local out = s;
    local i = #s;
    while (i < width) do
        out = __add(out, " ");
        i = __add(i, 1);
    end
    return out
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

function parseIntStr(str)
    local i = 0;
    local neg = false;
    if ((#str > 0) and (__slice(str, 0, 1) == "-")) then
        neg = true;
        i = 1;
    end
    local n = 0;
    local digits = {["0"]=0, ["1"]=1, ["2"]=2, ["3"]=3, ["4"]=4, ["5"]=5, ["6"]=6, ["7"]=7, ["8"]=8, ["9"]=9};
    while (i < #str) do
        n = __add((n * 10), __index(digits, __slice(str, i, __add(i, 1))));
        i = __add(i, 1);
    end
    if neg then
        n = -n;
    end
    return n
end

function isDigits(s)
    if (#s == 0) then
        return false
    end
    local i = 0;
    while (i < #s) do
        local ch = __slice(s, i, __add(i, 1));
        if ((ch < "0") or (ch > "9")) then
            return false
        end
        i = __add(i, 1);
    end
    return true
end

function readTable(table)
    local toks = fields(table);
    local cmds = {};
    local mins = {};
    local i = 0;
    while (i < #toks) do
        local cmd = __index(toks, i);
        local minlen = #cmd;
        i = __add(i, 1);
        if ((i < #toks) and isDigits(__index(toks, i))) then
            local num = parseIntStr(__index(toks, i));
            if ((num >= 1) and (num < #cmd)) then
                minlen = num;
                i = __add(i, 1);
            end
        end
        cmds = __append(cmds, cmd);
        mins = __append(mins, minlen);
    end
    return {["commands"]=cmds, ["mins"]=mins}
end

function validate(commands, mins, words)
    local results = {};
    local wi = 0;
    while (wi < #words) do
        local w = __index(words, wi);
        local found = false;
        local wlen = #w;
        local ci = 0;
        while (ci < #commands) do
            local cmd = __index(commands, ci);
            if (((__index(mins, ci) ~= 0) and (wlen >= __index(mins, ci))) and (wlen <= #cmd)) then
                local c = string.upper(cmd);
                local ww = string.upper(w);
                if (__slice(c, 0, wlen) == ww) then
                    results = __append(results, c);
                    found = true;
                    break
                end
            end
            ci = __add(ci, 1);
        end
        if not found then
            results = __append(results, "*error*");
        end
        wi = __add(wi, 1);
    end
    return results
end

function main()
    local table = (((((((("" .. "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") .. "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") .. "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") .. "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") .. "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") .. "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") .. "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") .. "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ");
    local sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin";
    local tbl = readTable(table);
    local commands = tbl["commands"];
    local mins = tbl["mins"];
    local words = fields(sentence);
    local results = validate(commands, mins, words);
    local out1 = "user words:";
    local k = 0;
    while (k < #words) do
        out1 = __add(out1, " ");
        if (k < (#words - 1)) then
            out1 = __add(out1, padRight(__index(words, k), #__index(results, k)));
        else
            out1 = __add(out1, __index(words, k));
        end
        k = __add(k, 1);
    end
    print(tostring(out1));
    print(("full words: " .. join(results, " ")));
end

main();
