function __load(path, opts)
    local fmt = 'json'
    if opts and opts['format'] then fmt = opts['format'] end
    local f
    if path and path ~= '' and path ~= '-' and not string.match(path, '^/') then
        local base = (arg and arg[0]) and string.match(arg[0], '(.*/)') or ''
        local try = base .. path
        f = io.open(try, 'r')
        if f then path = try else
            local root = os.getenv('MOCHI_ROOT')
            if root then
                local clean = path
                while string.sub(clean, 1, 3) == '../' do clean = string.sub(clean, 4) end
                try = root .. '/' .. clean
                f = io.open(try, 'r')
                if f then path = try end
            end
        end
    end
    if not f then
        if not path or path == '' or path == '-' then
            f = io.stdin
        else
            local err; f, err = io.open(path, 'r'); if not f then error(err) end
        end
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
Person = {}
Person.__index = Person
function Person.new(o)
    o = o or {}
    setmetatable(o, Person)
    return o
end

people = __load("../interpreter/valid/people.yaml", {["format"]="yaml"})
adults = (function()
    local _res = {}
    for _, p in ipairs(people) do
        if (p.age >= 18) then
            _res[#_res+1] = {["name"]=p.name, ["email"]=p.email}
        end
    end
    return _res
end)()
for _, a in ipairs(adults) do
    print(a.name, a.email)
    ::__continue0::
end
