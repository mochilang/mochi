function __save(rows, path, opts)
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
people = {{["name"]="Alice", ["age"]=30}, {["name"]="Bob", ["age"]=25}}
__save(people, "-", {["format"]="jsonl"})
