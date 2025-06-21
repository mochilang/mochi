package luacode

import "sort"

// Runtime helper functions injected into generated Lua programs.
const (
	helperPrint = "function __print(...)\n" +
		"    local args = {...}\n" +
		"    for i, a in ipairs(args) do\n" +
		"        if i > 1 then io.write(' ') end\n" +
		"        io.write(tostring(a))\n" +
		"    end\n" +
		"    io.write('\\n')\n" +
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
		"    return sum / #items\n" +
		"end\n"

	helperJson = "function __json(v)\n" +
		"    local ok, json = pcall(require, 'json')\n" +
		"    if not ok then error('json library not found') end\n" +
		"    print(json.encode(v))\n" +
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
		"    if not path or path == '' or path == '-' then\n" +
		"        f = io.stdin\n" +
		"    else\n" +
		"        local err; f, err = io.open(path, 'r'); if not f then error(err) end\n" +
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
		"        local ok, json = pcall(require, 'json')\n" +
		"        if not ok then error('json library not found') end\n" +
		"        local lines = {}\n" +
		"        for _, row in ipairs(rows) do\n" +
		"            table.insert(lines, json.encode(row))\n" +
		"        end\n" +
		"        data = table.concat(lines, '\n')\n" +
		"    elseif fmt == 'csv' then\n" +
		"        local lines = {}\n" +
		"        for _, row in ipairs(rows) do\n" +
		"            table.insert(lines, table.concat(row, ','))\n" +
		"        end\n" +
		"        data = table.concat(lines, '\n')\n" +
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
)

var helperMap = map[string]string{
	"print":       helperPrint,
	"iter":        helperIter,
	"div":         helperDiv,
	"add":         helperAdd,
	"eq":          helperEq,
	"contains":    helperContains,
	"input":       helperInput,
	"count":       helperCount,
	"avg":         helperAvg,
	"json":        helperJson,
	"eval":        helperEval,
	"index":       helperIndex,
	"indexString": helperIndexString,
	"slice":       helperSlice,
	"gen_text":    helperGenText,
	"gen_embed":   helperGenEmbed,
	"gen_struct":  helperGenStruct,
	"fetch":       helperFetch,
	"load":        helperLoad,
	"save":        helperSave,
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
