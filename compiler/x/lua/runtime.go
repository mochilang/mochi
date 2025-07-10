//go:build slow

package luacode

import "sort"

// Runtime helper functions injected into generated Lua programs.
const (
	helperRunTests = "function __run_tests(tests)\n" +
		"    local function format_duration(d)\n" +
		"        if d < 1e-6 then return string.format('%dns', math.floor(d*1e9)) end\n" +
		"        if d < 1e-3 then return string.format('%.1fµs', d*1e6) end\n" +
		"        if d < 1 then return string.format('%.1fms', d*1e3) end\n" +
		"        return string.format('%.2fs', d)\n" +
		"    end\n" +
		"    local failures = 0\n" +
		"    for _, t in ipairs(tests) do\n" +
		"        io.write('   test ' .. t.name .. ' ...')\n" +
		"        local start = os.clock()\n" +
		"        local ok, err = pcall(t.fn)\n" +
		"        local dur = os.clock() - start\n" +
		"        if ok then\n" +
		"            io.write(' ok (' .. format_duration(dur) .. ')\\n')\n" +
		"        else\n" +
		"            io.write(' fail ' .. tostring(err) .. ' (' .. format_duration(dur) .. ')\\n')\n" +
		"            failures = failures + 1\n" +
		"        end\n" +
		"    end\n" +
		"    if failures > 0 then\n" +
		"        io.write('\\n[FAIL] ' .. failures .. ' test(s) failed.\\n')\n" +
		"    end\n" +
		"end\n"

	helperPrint = "function __print(...)\n" +
		"    local args = {...}\n" +
		"    local parts = {}\n" +
		"    for i,a in ipairs(args) do\n" +
		"        if a ~= nil and a ~= '' then parts[#parts+1] = tostring(a) end\n" +
		"    end\n" +
		"    print(table.concat(parts, ' '))\n" +
		"end\n"

	helperIter = "function __iter(obj)\n" +
		"    if type(obj) == 'table' then\n" +
		"        if obj[1] ~= nil or #obj > 0 then\n" +
		"            local i = 0\n" +
		"            local n = #obj\n" +
		"            return function()\n" +
		"                i = i + 1\n" +
		"                if i <= n then return i, obj[i] end\n" +
		"            end\n" +
		"        else\n" +
		"            return pairs(obj)\n" +
		"        end\n" +
		"    elseif type(obj) == 'string' then\n" +
		"        local i = 0\n" +
		"        local n = #obj\n" +
		"        return function()\n" +
		"            i = i + 1\n" +
		"            if i <= n then return i, string.sub(obj, i, i) end\n" +
		"        end\n" +
		"    else\n" +
		"        return function() return nil end\n" +
		"    end\n" +
		"end\n"

	helperDiv = "function __div(a, b)\n" +
		"    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then\n" +
		"        return a // b\n" +
		"    end\n" +
		"    return a / b\n" +
		"end\n"

	helperAdd = "function __add(a, b)\n" +
		"    if type(a) == 'table' and type(b) == 'table' then\n" +
		"        local out = {}\n" +
		"        for i = 1, #a do out[#out+1] = a[i] end\n" +
		"        for i = 1, #b do out[#out+1] = b[i] end\n" +
		"        return out\n" +
		"    elseif type(a) == 'string' or type(b) == 'string' then\n" +
		"        return tostring(a) .. tostring(b)\n" +
		"    else\n" +
		"        return a + b\n" +
		"    end\n" +
		"end\n"

	helperEq = "function __eq(a, b)\n" +
		"    if type(a) ~= type(b) then return false end\n" +
		"    if type(a) == 'number' then return math.abs(a-b) < 1e-9 end\n" +
		"    if type(a) ~= 'table' then return a == b end\n" +
		"    if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then\n" +
		"        if #a ~= #b then return false end\n" +
		"        for i = 1, #a do if not __eq(a[i], b[i]) then return false end end\n" +
		"        return true\n" +
		"    end\n" +
		"    for k, v in pairs(a) do if not __eq(v, b[k]) then return false end end\n" +
		"    for k, _ in pairs(b) do if a[k] == nil then return false end end\n" +
		"    return true\n" +
		"end\n"

	helperContains = "function __contains(container, item)\n" +
		"    if type(container) == 'table' then\n" +
		"        if container[1] ~= nil or #container > 0 then\n" +
		"            for _, v in ipairs(container) do\n" +
		"                if v == item then return true end\n" +
		"            end\n" +
		"            return false\n" +
		"        else\n" +
		"            return container[item] ~= nil\n" +
		"        end\n" +
		"    elseif type(container) == 'string' then\n" +
		"        return string.find(container, item, 1, true) ~= nil\n" +
		"    else\n" +
		"        return false\n" +
		"    end\n" +
		"end\n"

	helperInput = "function __input()\n" +
		"    local line = io.read('*l')\n" +
		"    if line == nil then return '' end\n" +
		"    return line\n" +
		"end\n"

	helperCount = "function __count(v)\n" +
		"    if type(v) == 'table' then\n" +
		"        if v.items ~= nil then return #v.items end\n" +
		"        if v[1] ~= nil or #v > 0 then return #v end\n" +
		"        local n = 0\n" +
		"        for _ in pairs(v) do n = n + 1 end\n" +
		"        return n\n" +
		"    elseif type(v) == 'string' then\n" +
		"        return #v\n" +
		"    else\n" +
		"        error('count() expects list or group')\n" +
		"    end\n" +
		"end\n"

	helperExists = "function __exists(v)\n" +
		"    if type(v) == 'table' then\n" +
		"        if v.items ~= nil then return #v.items > 0 end\n" +
		"        if v[1] ~= nil or #v > 0 then return #v > 0 end\n" +
		"        return next(v) ~= nil\n" +
		"    elseif type(v) == 'string' then\n" +
		"        return #v > 0\n" +
		"    else\n" +
		"        return false\n" +
		"    end\n" +
		"end\n"

	helperAvg = "function __avg(v)\n" +
		"    local items\n" +
		"    if type(v) == 'table' and v.items ~= nil then\n" +
		"        items = v.items\n" +
		"    elseif type(v) == 'table' then\n" +
		"        items = v\n" +
		"    else\n" +
		"        error('avg() expects list or group')\n" +
		"    end\n" +
		"    if #items == 0 then return 0 end\n" +
		"    local sum = 0\n" +
		"    for _, it in ipairs(items) do sum = sum + it end\n" +
		"    local res = sum / #items\n" +
		"    if res == math.floor(res) then return math.floor(res) end\n" +
		"    return res\n" +
		"end\n"

	helperSum = "function __sum(v)\n" +
		"    local items\n" +
		"    if type(v) == 'table' and v.items ~= nil then\n" +
		"        items = v.items\n" +
		"    elseif type(v) == 'table' then\n" +
		"        items = v\n" +
		"    else\n" +
		"        error('sum() expects list or group')\n" +
		"    end\n" +
		"    local sum = 0\n" +
		"    for _, it in ipairs(items) do sum = sum + it end\n" +
		"    return sum\n" +
		"end\n"

	helperMin = "function __min(v)\n" +
		"    local items\n" +
		"    if type(v) == 'table' and v.items ~= nil then\n" +
		"        items = v.items\n" +
		"    elseif type(v) == 'table' then\n" +
		"        items = v\n" +
		"    else\n" +
		"        error('min() expects list or group')\n" +
		"    end\n" +
		"    if #items == 0 then return 0 end\n" +
		"    local m = items[1]\n" +
		"    if type(m) == 'string' then\n" +
		"        for i=2,#items do\n" +
		"            local it = items[i]\n" +
		"            if type(it) == 'string' and it < m then m = it end\n" +
		"        end\n" +
		"        return m\n" +
		"    else\n" +
		"        m = tonumber(m)\n" +
		"        for i=2,#items do\n" +
		"            local n = tonumber(items[i])\n" +
		"            if n < m then m = n end\n" +
		"        end\n" +
		"        return m\n" +
		"    end\n" +
		"end\n"

	helperMax = "function __max(v)\n" +
		"    local items\n" +
		"    if type(v) == 'table' and v.items ~= nil then\n" +
		"        items = v.items\n" +
		"    elseif type(v) == 'table' then\n" +
		"        items = v\n" +
		"    else\n" +
		"        error('max() expects list or group')\n" +
		"    end\n" +
		"    if #items == 0 then return 0 end\n" +
		"    local m = items[1]\n" +
		"    if type(m) == 'string' then\n" +
		"        for i=2,#items do\n" +
		"            local it = items[i]\n" +
		"            if type(it) == 'string' and it > m then m = it end\n" +
		"        end\n" +
		"        return m\n" +
		"    else\n" +
		"        m = tonumber(m)\n" +
		"        for i=2,#items do\n" +
		"            local n = tonumber(items[i])\n" +
		"            if n > m then m = n end\n" +
		"        end\n" +
		"        return m\n" +
		"    end\n" +
		"end\n"

	helperFirst = "function __first(v)\n" +
		"    if type(v) == 'table' then\n" +
		"        if v.items ~= nil then\n" +
		"            if #v.items == 0 then return nil end\n" +
		"            return v.items[1]\n" +
		"        end\n" +
		"        if #v == 0 then return nil end\n" +
		"        return v[1]\n" +
		"    end\n" +
		"    return nil\n" +
		"end\n"

	helperConcat = "function __concat(a, b)\n" +
		"    local res = {}\n" +
		"    if a then for i=1,#a do res[#res+1] = a[i] end end\n" +
		"    if b then for i=1,#b do res[#res+1] = b[i] end end\n" +
		"    return res\n" +
		"end\n"

	helperReverseString = "function __reverse_string(s)\n" +
		"    return string.reverse(s)\n" +
		"end\n"

	helperReverseList = "function __reverse_list(lst)\n" +
		"    local out = {}\n" +
		"    for i=#lst,1,-1 do out[#out+1] = lst[i] end\n" +
		"    return out\n" +
		"end\n"
	helperAppend = "function __append(lst, v)\n" +
		"    local out = {}\n" +
		"    if lst then for i = 1, #lst do out[#out+1] = lst[i] end end\n" +
		"    out[#out+1] = v\n" +
		"    return out\n" +
		"end\n"

	helperValues = "function __values(m)\n" +
		"    if type(m) ~= 'table' then error('values() expects map') end\n" +
		"    local keys = {}\n" +
		"    for k in pairs(m) do keys[#keys+1] = k end\n" +
		"    table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)\n" +
		"    local out = {}\n" +
		"    for _, k in ipairs(keys) do out[#out+1] = m[k] end\n" +
		"    return out\n" +
		"end\n"

	helperReduce = "function __reduce(src, fn, acc)\n" +
		"    for _, it in ipairs(src) do\n" +
		"        acc = fn(acc, it)\n" +
		"    end\n" +
		"    return acc\n" +
		"end\n"

	helperJson = "function __json(v)\n" +
		"    if type(v) == 'table' and next(v) == nil then print('[]'); return end\n" +
		"    local function sort(x)\n" +
		"        if type(x) ~= 'table' then return x end\n" +
		"        if x[1] ~= nil or #x > 0 then\n" +
		"            local out = {}\n" +
		"            for i=1,#x do out[i] = sort(x[i]) end\n" +
		"            return out\n" +
		"        end\n" +
		"        local keys = {}\n" +
		"        for k in pairs(x) do keys[#keys+1] = k end\n" +
		"        table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)\n" +
		"        local out = {}\n" +
		"        for _,k in ipairs(keys) do out[k] = sort(x[k]) end\n" +
		"        return out\n" +
		"    end\n" +
		"    local ok, json = pcall(require, 'json')\n" +
		"    if not ok then ok, json = pcall(require, 'cjson') end\n" +
		"    if ok then\n" +
		"        print(json.encode(sort(v)))\n" +
		"        return\n" +
		"    end\n" +
		"    local function enc(x)\n" +
		"        local t = type(x)\n" +
		"        if t == 'nil' then\n" +
		"            return 'null'\n" +
		"        elseif t == 'boolean' or t == 'number' then\n" +
		"            return tostring(x)\n" +
		"        elseif t == 'string' then\n" +
		"            return string.format('%q', x)\n" +
		"        elseif t == 'table' then\n" +
		"            if x[1] ~= nil or #x > 0 then\n" +
		"                local parts = {}\n" +
		"                for i=1,#x do parts[#parts+1] = enc(x[i]) end\n" +
		"                return '['..table.concat(parts, ',')..']'\n" +
		"            else\n" +
		"                local keys = {}\n" +
		"                for k in pairs(x) do keys[#keys+1] = k end\n" +
		"                table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)\n" +
		"                local parts = {}\n" +
		"                for _,k in ipairs(keys) do parts[#parts+1] = enc(k)..':'..enc(x[k]) end\n" +
		"                return '{'..table.concat(parts, ',')..'}'\n" +
		"            end\n" +
		"        else\n" +
		"            return 'null'\n" +
		"        end\n" +
		"    end\n" +
		"    print(enc(sort(v)))\n" +
		"end\n"

	helperEval = "function __eval(code)\n" +
		"    local f, err = load(code)\n" +
		"    if not f then error(err) end\n" +
		"    return f()\n" +
		"end\n"

	helperIndex = "function __index(obj, i)\n" +
		"    if type(obj) == 'string' then\n" +
		"        return __indexString(obj, i)\n" +
		"    elseif type(obj) == 'table' then\n" +
		"        if obj[1] ~= nil or #obj > 0 then\n" +
		"            return obj[(i)+1]\n" +
		"        else\n" +
		"            return obj[i]\n" +
		"        end\n" +
		"    else\n" +
		"        error('cannot index')\n" +
		"    end\n" +
		"end\n"

	helperIndexString = "function __indexString(s, i)\n" +
		"    local len = #s\n" +
		"    if i < 0 then\n" +
		"        i = len + i + 1\n" +
		"    else\n" +
		"        i = i + 1\n" +
		"    end\n" +
		"    if i < 1 or i > len then error('index out of range') end\n" +
		"    return string.sub(s, i, i)\n" +
		"end\n"

	helperSlice = "function __slice(obj, i, j)\n" +
		"    if i == nil then i = 0 end\n" +
		"    if type(obj) == 'string' then\n" +
		"        local len = #obj\n" +
		"        if j == nil then j = len end\n" +
		"        if i < 0 then i = len + i end\n" +
		"        if j < 0 then j = len + j end\n" +
		"        if i < 0 then i = 0 end\n" +
		"        if j > len then j = len end\n" +
		"        return string.sub(obj, i+1, j)\n" +
		"    elseif type(obj) == 'table' then\n" +
		"        local len = #obj\n" +
		"        if j == nil then j = len end\n" +
		"        if i < 0 then i = len + i end\n" +
		"        if j < 0 then j = len + j end\n" +
		"        if i < 0 then i = 0 end\n" +
		"        if j > len then j = len end\n" +
		"        local out = {}\n" +
		"        for k = i+1, j do\n" +
		"            out[#out+1] = obj[k]\n" +
		"        end\n" +
		"        return out\n" +
		"    else\n" +
		"        return {}\n" +
		"    end\n" +
		"end\n"

	helperUnionAll = "function __union_all(a, b)\n" +
		"    local res = {}\n" +
		"    if a then for _, v in ipairs(a) do res[#res+1] = v end end\n" +
		"    if b then for _, v in ipairs(b) do res[#res+1] = v end end\n" +
		"    return res\n" +
		"end\n"

	helperUnion = "function __union(a, b)\n" +
		"    local res = {}\n" +
		"    local function add(lst)\n" +
		"        if lst then\n" +
		"            for _, v in ipairs(lst) do\n" +
		"                local dup = false\n" +
		"                for _, w in ipairs(res) do\n" +
		"                    if __eq(v, w) then dup = true break end\n" +
		"                end\n" +
		"                if not dup then res[#res+1] = v end\n" +
		"            end\n" +
		"        end\n" +
		"    end\n" +
		"    add(a); add(b);\n" +
		"    return res\n" +
		"end\n"

	helperExcept = "function __except(a, b)\n" +
		"    local res = {}\n" +
		"    if a then\n" +
		"        for _, v in ipairs(a) do\n" +
		"            local found = false\n" +
		"            if b then\n" +
		"                for _, w in ipairs(b) do\n" +
		"                    if __eq(v, w) then found = true break end\n" +
		"                end\n" +
		"            end\n" +
		"            if not found then res[#res+1] = v end\n" +
		"        end\n" +
		"    end\n" +
		"    return res\n" +
		"end\n"

	helperIntersect = "function __intersect(a, b)\n" +
		"    local res = {}\n" +
		"    if a and b then\n" +
		"        for _, v in ipairs(a) do\n" +
		"            for _, w in ipairs(b) do\n" +
		"                if __eq(v, w) then\n" +
		"                    local dup = false\n" +
		"                    for _, r in ipairs(res) do if __eq(r, v) then dup = true break end end\n" +
		"                    if not dup then res[#res+1] = v end\n" +
		"                    break\n" +
		"                end\n" +
		"            end\n" +
		"        end\n" +
		"    end\n" +
		"    return res\n" +
		"end\n"

	helperGenText = "function __gen_text(prompt, model, params)\n" +
		"    return prompt\n" +
		"end\n"

	helperGenEmbed = "function __gen_embed(text, model, params)\n" +
		"    local out = {}\n" +
		"    for i = 1, #text do\n" +
		"        out[#out+1] = string.byte(text, i)\n" +
		"    end\n" +
		"    return out\n" +
		"end\n"

	helperGenStruct = "function __gen_struct(prompt, model, params)\n" +
		"    local f = load('return ' .. prompt:gsub('\"(%w+)\"%s*:', '%1='))\n" +
		"    if f then\n" +
		"        local ok, res = pcall(f)\n" +
		"        if ok and type(res) == 'table' then return res end\n" +
		"    end\n" +
		"    return {}\n" +
		"end\n"

	helperFetch = "function __fetch(url, opts)\n" +
		"    local args = {'-s'}\n" +
		"    local method = 'GET'\n" +
		"    if opts and opts['method'] then method = tostring(opts['method']) end\n" +
		"    table.insert(args, '-X')\n" +
		"    table.insert(args, method)\n" +
		"    if opts and opts['headers'] then\n" +
		"        for k,v in pairs(opts['headers']) do\n" +
		"            table.insert(args, '-H')\n" +
		"            table.insert(args, k .. ': ' .. tostring(v))\n" +
		"        end\n" +
		"    end\n" +
		"    if opts and opts['query'] then\n" +
		"        local qs = {}\n" +
		"        for k,v in pairs(opts['query']) do\n" +
		"            table.insert(qs, k .. '=' .. tostring(v))\n" +
		"        end\n" +
		"        local sep = string.find(url, '?') and '&' or '?'\n" +
		"        url = url .. sep .. table.concat(qs, '&')\n" +
		"    end\n" +
		"    if opts and opts['body'] ~= nil then\n" +
		"        local ok, json = pcall(require, 'json')\n" +
		"        if not ok then error('json library not found') end\n" +
		"        table.insert(args, '-d')\n" +
		"        table.insert(args, json.encode(opts['body']))\n" +
		"    end\n" +
		"    if opts and opts['timeout'] then\n" +
		"        table.insert(args, '--max-time')\n" +
		"        table.insert(args, tostring(opts['timeout']))\n" +
		"    end\n" +
		"    table.insert(args, url)\n" +
		"    local cmd = 'curl ' .. table.concat(args, ' ')\n" +
		"    local f = assert(io.popen(cmd))\n" +
		"    local data = f:read('*a')\n" +
		"    f:close()\n" +
		"    local ok, json = pcall(require, 'json')\n" +
		"    if not ok then error('json library not found') end\n" +
		"    return json.decode(data)\n" +
		"end\n"

	helperLoad = "function __load(path, opts)\n" +
		"    local fmt = 'json'\n" +
		"    if opts and opts['format'] then fmt = opts['format'] end\n" +
		"    local f\n" +
		"    if path and path ~= '' and path ~= '-' and not string.match(path, '^/') then\n" +
		"        local base = (arg and arg[0]) and string.match(arg[0], '(.*/)') or ''\n" +
		"        local try = base .. path\n" +
		"        f = io.open(try, 'r')\n" +
		"        if f then path = try else\n" +
		"            local root = os.getenv('MOCHI_ROOT')\n" +
		"            if root then\n" +
		"                local clean = path\n" +
		"                while string.sub(clean, 1, 3) == '../' do clean = string.sub(clean, 4) end\n" +
		"                try = root .. '/tests/' .. clean\n" +
		"                f = io.open(try, 'r')\n" +
		"                if not f then\n" +
		"                    try = root .. '/' .. clean\n" +
		"                    f = io.open(try, 'r')\n" +
		"                end\n" +
		"                if f then path = try end\n" +
		"            end\n" +
		"        end\n" +
		"    end\n" +
		"    if not f then\n" +
		"        if not path or path == '' or path == '-' then\n" +
		"            f = io.stdin\n" +
		"        else\n" +
		"            local err; f, err = io.open(path, 'r'); if not f then error(err) end\n" +
		"        end\n" +
		"    end\n" +
		"    local data = f:read('*a')\n" +
		"    if f ~= io.stdin then f:close() end\n" +
		"    local res\n" +
		"    if fmt == 'json' then\n" +
		"        local ok, json = pcall(require, 'json')\n" +
		"        if not ok then error('json library not found') end\n" +
		"        res = json.decode(data)\n" +
		"    elseif fmt == 'yaml' then\n" +
		"        local ok, yaml = pcall(require, 'yaml')\n" +
		"        if not ok then ok, yaml = pcall(require, 'lyaml') end\n" +
		"        if not ok then error('yaml library not found') end\n" +
		"        res = yaml.load(data)\n" +
		"    elseif fmt == 'jsonl' then\n" +
		"        local ok, json = pcall(require, 'json')\n" +
		"        if not ok then error('json library not found') end\n" +
		"        res = {}\n" +
		"        for line in string.gmatch(data, '[^\\n]+') do\n" +
		"            if line ~= '' then table.insert(res, json.decode(line)) end\n" +
		"        end\n" +
		"    elseif fmt == 'csv' then\n" +
		"        res = {}\n" +
		"        for line in string.gmatch(data, '[^\\n]+') do\n" +
		"            local row = {}\n" +
		"            for field in string.gmatch(line, '[^,]+') do table.insert(row, field) end\n" +
		"            table.insert(res, row)\n" +
		"        end\n" +
		"    else\n" +
		"        error('unsupported format: '..fmt)\n" +
		"    end\n" +
		"    if type(res) ~= 'table' then return {} end\n" +
		"    if res[1] then return res end\n" +
		"    return {res}\n" +
		"end\n"

	helperSave = "function __save(rows, path, opts)\n" +
		"    local fmt = 'json'\n" +
		"    if opts and opts['format'] then fmt = opts['format'] end\n" +
		"    local data\n" +
		"    if fmt == 'json' then\n" +
		"        local ok, json = pcall(require, 'json')\n" +
		"        if not ok then error('json library not found') end\n" +
		"        data = json.encode(rows)\n" +
		"    elseif fmt == 'yaml' then\n" +
		"        local ok, yaml = pcall(require, 'yaml')\n" +
		"        if not ok then ok, yaml = pcall(require, 'lyaml') end\n" +
		"        if not ok then error('yaml library not found') end\n" +
		"        if yaml.dump then data = yaml.dump(rows) else data = yaml.encode(rows) end\n" +
		"    elseif fmt == 'jsonl' then\n" +
		"        local function enc(v)\n" +
		"            if type(v)=='table' then\n" +
		"                local keys={}\n" +
		"                for k in pairs(v) do keys[#keys+1]=k end\n" +
		"                table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)\n" +
		"                local parts={'{'}\n" +
		"                for i,k in ipairs(keys) do\n" +
		"                    if i>1 then parts[#parts+1]=',' end\n" +
		"                    parts[#parts+1]=string.format('%q:',k)\n" +
		"                    parts[#parts+1]=enc(v[k])\n" +
		"                end\n" +
		"                parts[#parts+1]='}'\n" +
		"                return table.concat(parts)\n" +
		"            elseif type(v)=='string' then\n" +
		"                return string.format('%q',v)\n" +
		"            else\n" +
		"                return tostring(v)\n" +
		"            end\n" +
		"        end\n" +
		"        local lines={}\n" +
		"        for _,row in ipairs(rows) do lines[#lines+1]=enc(row) end\n" +
		"        data=table.concat(lines,'\\n')\n" +
		"    elseif fmt == 'csv' then\n" +
		"        local lines = {}\n" +
		"        for _, row in ipairs(rows) do\n" +
		"            table.insert(lines, table.concat(row, ','))\n" +
		"        end\n" +
		"        data = table.concat(lines, '\\n')\n" +
		"    else\n" +
		"        error('unsupported format: '..fmt)\n" +
		"    end\n" +
		"    local f\n" +
		"    if not path or path == '' or path == '-' then\n" +
		"        f = io.stdout\n" +
		"    else\n" +
		"        local err; f, err = io.open(path, 'w'); if not f then error(err) end\n" +
		"    end\n" +
		"    f:write(data)\n" +
		"    if f ~= io.stdout then f:close() end\n" +
		"end\n"

	helperGroup = "_Group = {}\n" +
		"function _Group.new(k)\n" +
		"    return {key = k, items = {}}\n" +
		"end\n"

	helperGroupBy = "function __group_by(src, keyfn)\n" +
		"    local groups = {}\n" +
		"    local order = {}\n" +
		"    for _, it in ipairs(src) do\n" +
		"        local key = keyfn(it)\n" +
		"        local ks\n" +
		"        if type(key) == 'table' then\n" +
		"            local fields = {}\n" +
		"            for k,_ in pairs(key) do fields[#fields+1] = k end\n" +
		"            table.sort(fields)\n" +
		"            local parts = {}\n" +
		"            for _,k in ipairs(fields) do parts[#parts+1] = tostring(k)..'='..tostring(key[k]) end\n" +
		"            ks = table.concat(parts, ',')\n" +
		"        else\n" +
		"            ks = tostring(key)\n" +
		"        end\n" +
		"        local g = groups[ks]\n" +
		"        if not g then\n" +
		"            g = _Group.new(key)\n" +
		"            groups[ks] = g\n" +
		"            order[#order+1] = ks\n" +
		"        end\n" +
		"        table.insert(g.items, it)\n" +
		"    end\n" +
		"    local res = {}\n" +
		"    for _, ks in ipairs(order) do\n" +
		"        res[#res+1] = groups[ks]\n" +
		"    end\n" +
		"    return res\n" +
		"end\n"

	helperGroupByRows = "function __group_by_rows(rows, keyfn, valfn)\n" +
		"    local groups = {}\n" +
		"    local order = {}\n" +
		"    for _, r in ipairs(rows) do\n" +
		"        local key = keyfn(table.unpack(r))\n" +
		"        local ks\n" +
		"        if type(key) == 'table' then\n" +
		"            local fields = {}\n" +
		"            for k,_ in pairs(key) do fields[#fields+1] = k end\n" +
		"            table.sort(fields)\n" +
		"            local parts = {}\n" +
		"            for _,k in ipairs(fields) do parts[#parts+1] = tostring(k)..'='..tostring(key[k]) end\n" +
		"            ks = table.concat(parts, ',')\n" +
		"        else\n" +
		"            ks = tostring(key)\n" +
		"        end\n" +
		"        local g = groups[ks]\n" +
		"        if not g then\n" +
		"            g = _Group.new(key)\n" +
		"            groups[ks] = g\n" +
		"            order[#order+1] = ks\n" +
		"        end\n" +
		"        table.insert(g.items, valfn(table.unpack(r)))\n" +
		"    end\n" +
		"    local res = {}\n" +
		"    for _, ks in ipairs(order) do\n" +
		"        res[#res+1] = groups[ks]\n" +
		"    end\n" +
		"    return res\n" +
		"end\n"

	helperMerge = "function __merge(...)\n" +
		"    local res = {}\n" +
		"    for i=1,select('#', ...) do\n" +
		"        local t = select(i, ...)\n" +
		"        if type(t) == 'table' then\n" +
		"            for k,v in pairs(t) do res[k] = v end\n" +
		"        end\n" +
		"    end\n" +
		"    return res\n" +
		"end\n"

	helperQuery = "function __query(src, joins, opts)\n" +
		"    local whereFn = opts.where\n" +
		"    local items = {}\n" +
		"    if #joins == 0 and whereFn then\n" +
		"        for _, v in ipairs(src) do if whereFn(v) then items[#items+1] = {v} end end\n" +
		"    else\n" +
		"        for _, v in ipairs(src) do items[#items+1] = {v} end\n" +
		"    end\n" +
		"    for ji, j in ipairs(joins) do\n" +
		"        local joined = {}\n" +
		"        local jitems = j.items or {}\n" +
		"        if j.right and j.left then\n" +
		"            local matched = {}\n" +
		"            for _, left in ipairs(items) do\n" +
		"                local m = false\n" +
		"                for ri, right in ipairs(jitems) do\n" +
		"                    local keep = true\n" +
		"                    if j.on then\n" +
		"                        local args = {table.unpack(left)}\n" +
		"                        args[#args+1] = right\n" +
		"                        keep = j.on(table.unpack(args))\n" +
		"                    end\n" +
		"                    if keep then\n" +
		"                        m = true; matched[ri] = true\n" +
		"                        local row = {table.unpack(left)}\n" +
		"                        row[#row+1] = right\n" +
		"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                        else\n" +
		"                            joined[#joined+1] = row\n" +
		"                        end\n" +
		"                    end\n" +
		"                end\n" +
		"                if not m then\n" +
		"                    local row = {table.unpack(left)}\n" +
		"                    row[#row+1] = nil\n" +
		"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                    else\n" +
		"                        joined[#joined+1] = row\n" +
		"                    end\n" +
		"                end\n" +
		"            end\n" +
		"            for ri, right in ipairs(jitems) do\n" +
		"                if not matched[ri] then\n" +
		"                    local undef = {}\n" +
		"                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end\n" +
		"                    local row = {table.unpack(undef)}\n" +
		"                    row[#row+1] = right\n" +
		"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                    else\n" +
		"                        joined[#joined+1] = row\n" +
		"                    end\n" +
		"                end\n" +
		"            end\n" +
		"        elseif j.right then\n" +
		"            for _, right in ipairs(jitems) do\n" +
		"                local m = false\n" +
		"                for _, left in ipairs(items) do\n" +
		"                    local keep = true\n" +
		"                    if j.on then\n" +
		"                        local args = {table.unpack(left)}\n" +
		"                        args[#args+1] = right\n" +
		"                        keep = j.on(table.unpack(args))\n" +
		"                    end\n" +
		"                    if keep then\n" +
		"                        m = true\n" +
		"                        local row = {table.unpack(left)}\n" +
		"                        row[#row+1] = right\n" +
		"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                        else\n" +
		"                            joined[#joined+1] = row\n" +
		"                        end\n" +
		"                    end\n" +
		"                end\n" +
		"                if not m then\n" +
		"                    local undef = {}\n" +
		"                    if #items > 0 then for _=1,#items[1] do undef[#undef+1]=nil end end\n" +
		"                    local row = {table.unpack(undef)}\n" +
		"                    row[#row+1] = right\n" +
		"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                    else\n" +
		"                        joined[#joined+1] = row\n" +
		"                    end\n" +
		"                end\n" +
		"            end\n" +
		"        else\n" +
		"            for _, left in ipairs(items) do\n" +
		"                local m = false\n" +
		"                for _, right in ipairs(jitems) do\n" +
		"                    local keep = true\n" +
		"                    if j.on then\n" +
		"                        local args = {table.unpack(left)}\n" +
		"                        args[#args+1] = right\n" +
		"                        keep = j.on(table.unpack(args))\n" +
		"                    end\n" +
		"                    if keep then\n" +
		"                        m = true\n" +
		"                        local row = {table.unpack(left)}\n" +
		"                        row[#row+1] = right\n" +
		"                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                        else\n" +
		"                            joined[#joined+1] = row\n" +
		"                        end\n" +
		"                    end\n" +
		"                end\n" +
		"                if j.left and not m then\n" +
		"                    local row = {table.unpack(left)}\n" +
		"                    row[#row+1] = nil\n" +
		"                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then\n" +
		"                    else\n" +
		"                        joined[#joined+1] = row\n" +
		"                    end\n" +
		"                end\n" +
		"            end\n" +
		"        end\n" +
		"        items = joined\n" +
		"    end\n" +
		"    if opts.sortKey then\n" +
		"        local pairs = {}\n" +
		"        for _, it in ipairs(items) do pairs[#pairs+1] = {item=it, key=opts.sortKey(table.unpack(it))} end\n" +
		"        table.sort(pairs, function(a,b)\n" +
		"            local ak, bk = a.key, b.key\n" +
		"            if type(ak)=='number' and type(bk)=='number' then return ak < bk end\n" +
		"            if type(ak)=='string' and type(bk)=='string' then return ak < bk end\n" +
		"            return tostring(ak) < tostring(bk)\n" +
		"        end)\n" +
		"        items = {}\n" +
		"        for i,p in ipairs(pairs) do items[i] = p.item end\n" +
		"    end\n" +
		"    if opts.skip ~= nil then\n" +
		"        local n = opts.skip\n" +
		"        if n < #items then\n" +
		"            for i=1,n do table.remove(items,1) end\n" +
		"        else\n" +
		"            items = {}\n" +
		"        end\n" +
		"    end\n" +
		"    if opts.take ~= nil then\n" +
		"        local n = opts.take\n" +
		"        if n < #items then\n" +
		"            for i=#items, n+1, -1 do table.remove(items) end\n" +
		"        end\n" +
		"    end\n" +
		"    local res = {}\n" +
		"    for _, r in ipairs(items) do res[#res+1] = opts.selectFn(table.unpack(r)) end\n" +
		"    return res\n" +
		"end\n"
)

var helperMap = map[string]string{
	"run_tests":      helperRunTests,
	"print":          helperPrint,
	"iter":           helperIter,
	"div":            helperDiv,
	"add":            helperAdd,
	"eq":             helperEq,
	"contains":       helperContains,
	"input":          helperInput,
	"count":          helperCount,
	"exists":         helperExists,
	"avg":            helperAvg,
	"sum":            helperSum,
	"min":            helperMin,
	"max":            helperMax,
	"first":          helperFirst,
	"concat":         helperConcat,
	"append":         helperAppend,
	"values":         helperValues,
	"reduce":         helperReduce,
	"json":           helperJson,
	"eval":           helperEval,
	"index":          helperIndex,
	"indexString":    helperIndexString,
	"slice":          helperSlice,
	"reverse_string": helperReverseString,
	"reverse_list":   helperReverseList,
	"union_all":      helperUnionAll,
	"union":          helperUnion,
	"except":         helperExcept,
	"intersect":      helperIntersect,
	"gen_text":       helperGenText,
	"gen_embed":      helperGenEmbed,
	"gen_struct":     helperGenStruct,
	"fetch":          helperFetch,
	"load":           helperLoad,
	"save":           helperSave,
	"_Group":         helperGroup,
	"_group_by":      helperGroupBy,
	"_group_by_rows": helperGroupByRows,
	"merge":          helperMerge,
	"query":          helperQuery,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitHelpers() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
	}
}
