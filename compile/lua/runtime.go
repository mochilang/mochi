package luacode

import "sort"

// Runtime helper functions injected into generated programs.
const (
	helperPrint = `function __print(...)
    local args = {...}
    for i, a in ipairs(args) do
        if i > 1 then io.write(' ') end
        io.write(tostring(a))
    end
    io.write('\n')
end
`

	helperIter = `function __iter(obj)
    if type(obj) == 'table' then
        if obj[1] ~= nil or #obj > 0 then
            local i = 0
            local n = #obj
            return function()
                i = i + 1
                if i <= n then return i, obj[i] end
            end
        else
            return pairs(obj)
        end
    elseif type(obj) == 'string' then
        local i = 0
        local n = #obj
        return function()
            i = i + 1
            if i <= n then return i, string.sub(obj, i, i) end
        end
    else
        return function() return nil end
    end
end
`

	helperDiv = `function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
end
`

	helperAdd = `function __add(a, b)
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
`

	helperEq = `function __eq(a, b)
    if type(a) ~= type(b) then return false end
    if type(a) ~= 'table' then return a == b end
    if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then
        if #a ~= #b then return false end
        for i = 1, #a do if not __eq(a[i], b[i]) then return false end end
        return true
    end
    for k, v in pairs(a) do if not __eq(v, b[k]) then return false end end
    for k, _ in pairs(b) do if a[k] == nil then return false end end
    return true
end
`

	helperContains = `function __contains(container, item)
    if type(container) == 'table' then
        if container[1] ~= nil or #container > 0 then
            for _, v in ipairs(container) do
                if v == item then return true end
            end
            return false
        else
            return container[item] ~= nil
        end
    elseif type(container) == 'string' then
        return string.find(container, item, 1, true) ~= nil
    else
        return false
    end
end
`

	helperInput = `function __input()
    local line = io.read('*l')
    if line == nil then return '' end
    return line
end
`

	helperCount = `function __count(v)
    if type(v) == 'table' then
        if v.items ~= nil then return #v.items end
        if v[1] ~= nil or #v > 0 then return #v end
        local n = 0
        for _ in pairs(v) do n = n + 1 end
        return n
    elseif type(v) == 'string' then
        return #v
    else
        error('count() expects list or group')
    end
end
`

	helperAvg = `function __avg(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('avg() expects list or group')
    end
    if #items == 0 then return 0 end
    local sum = 0
    for _, it in ipairs(items) do sum = sum + it end
    return sum / #items
end
`

	helperIndex = `function __index(obj, i)
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
`

	helperIndexString = `function __indexString(s, i)
    local len = #s
    if i < 0 then
        i = len + i + 1
    else
        i = i + 1
    end
    if i < 1 or i > len then error('index out of range') end
    return string.sub(s, i, i)
end
`

	helperSlice = `function __slice(obj, i, j)
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
`

	helperGenText = `function __gen_text(prompt, model, params)
    return prompt
end
`

	helperGenEmbed = `function __gen_embed(text, model, params)
    local out = {}
    for i = 1, #text do
        out[#out+1] = string.byte(text, i)
    end
    return out
end
`

	helperGenStruct = `function __gen_struct(prompt, model, params)
    local f = load('return ' .. prompt:gsub('"(%w+)"%s*:', '%1='))
    if f then
        local ok, res = pcall(f)
        if ok and type(res) == 'table' then return res end
    end
    return {}
end
`

	helperFetch = `function __fetch(url, opts)
    local args = {'-s'}
    local method = 'GET'
    if opts and opts['method'] then method = tostring(opts['method']) end
    table.insert(args, '-X')
    table.insert(args, method)
    if opts and opts['headers'] then
        for k,v in pairs(opts['headers']) do
            table.insert(args, '-H')
            table.insert(args, k .. ': ' .. tostring(v))
        end
    end
    if opts and opts['query'] then
        local qs = {}
        for k,v in pairs(opts['query']) do
            table.insert(qs, k .. '=' .. tostring(v))
        end
        local sep = string.find(url, '?') and '&' or '?'
        url = url .. sep .. table.concat(qs, '&')
    end
    if opts and opts['body'] ~= nil then
        local ok, json = pcall(require, 'json')
        if not ok then error('json library not found') end
        table.insert(args, '-d')
        table.insert(args, json.encode(opts['body']))
    end
    if opts and opts['timeout'] then
        table.insert(args, '--max-time')
        table.insert(args, tostring(opts['timeout']))
    end
    table.insert(args, url)
    local cmd = 'curl ' .. table.concat(args, ' ')
    local f = assert(io.popen(cmd))
    local data = f:read('*a')
    f:close()
    local ok, json = pcall(require, 'json')
    if not ok then error('json library not found') end
    return json.decode(data)
end
`

	helperLoad = `function __load(path, opts)
    local fmt = 'json'
    if opts and opts['format'] then fmt = opts['format'] end
    local f
    if not path or path == '' or path == '-' then
        f = io.stdin
    else
        local err; f, err = io.open(path, 'r'); if not f then error(err) end
    end
    local data = f:read('*a')
    if f ~= io.stdin then f:close() end
    local res
    if fmt == 'json' then
        local ok, json = pcall(require, 'json')
        if not ok then error('json library not found') end
        res = json.decode(data)
    elseif fmt == 'yaml' then
        local ok, yaml = pcall(require, 'yaml')
        if not ok then ok, yaml = pcall(require, 'lyaml') end
        if not ok then error('yaml library not found') end
        res = yaml.load(data)
    elseif fmt == 'jsonl' then
        local ok, json = pcall(require, 'json')
        if not ok then error('json library not found') end
        res = {}
        for line in string.gmatch(data, '[^\n]+') do
            if line ~= '' then table.insert(res, json.decode(line)) end
        end
    elseif fmt == 'csv' then
        res = {}
        for line in string.gmatch(data, '[^\n]+') do
            local row = {}
            for field in string.gmatch(line, '[^,]+') do table.insert(row, field) end
            table.insert(res, row)
        end
    else
        error('unsupported format: '..fmt)
    end
    if type(res) ~= 'table' then return {} end
    if res[1] then return res end
    return {res}
end
`

	helperSave = `function __save(rows, path, opts)
    local fmt = 'json'
    if opts and opts['format'] then fmt = opts['format'] end
    local data
    if fmt == 'json' then
        local ok, json = pcall(require, 'json')
        if not ok then error('json library not found') end
        data = json.encode(rows)
    elseif fmt == 'yaml' then
        local ok, yaml = pcall(require, 'yaml')
        if not ok then ok, yaml = pcall(require, 'lyaml') end
        if not ok then error('yaml library not found') end
        if yaml.dump then data = yaml.dump(rows) else data = yaml.encode(rows) end
    elseif fmt == 'jsonl' then
        local ok, json = pcall(require, 'json')
        if not ok then error('json library not found') end
        local lines = {}
        for _, row in ipairs(rows) do
            table.insert(lines, json.encode(row))
        end
        data = table.concat(lines, '\n')
    elseif fmt == 'csv' then
        local lines = {}
        for _, row in ipairs(rows) do
            table.insert(lines, table.concat(row, ','))
        end
        data = table.concat(lines, '\n')
    else
        error('unsupported format: '..fmt)
    end
    local f
    if not path or path == '' or path == '-' then
        f = io.stdout
    else
        local err; f, err = io.open(path, 'w'); if not f then error(err) end
    end
    f:write(data)
    if f ~= io.stdout then f:close() end
end
`
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

func (c *Compiler) emitRuntime() {
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
