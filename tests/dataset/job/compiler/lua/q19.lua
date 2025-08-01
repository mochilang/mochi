-- Generated by Mochi compiler v0.10.25 on 2025-07-13T11:45:19Z
function __contains(container, item)
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
function __eq(a, b)
    if type(a) ~= type(b) then return false end
    if type(a) == 'number' then return math.abs(a-b) < 1e-9 end
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
function __json(v)
    if type(v) == 'table' and next(v) == nil then print('[]'); return end
    local function sort(x)
        if type(x) ~= 'table' then return x end
        if x[1] ~= nil or #x > 0 then
            local out = {}
            for i=1,#x do out[i] = sort(x[i]) end
            return out
        end
        local keys = {}
        for k in pairs(x) do keys[#keys+1] = k end
        table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)
        local out = {}
        for _,k in ipairs(keys) do out[k] = sort(x[k]) end
        return out
    end
    local ok, json = pcall(require, 'json')
    if not ok then ok, json = pcall(require, 'cjson') end
    if ok then
        print(json.encode(sort(v)))
        return
    end
    local function enc(x)
        local t = type(x)
        if t == 'nil' then
            return 'null'
        elseif t == 'boolean' or t == 'number' then
            return tostring(x)
        elseif t == 'string' then
            return string.format('%q', x)
        elseif t == 'table' then
            if x[1] ~= nil or #x > 0 then
                local parts = {}
                for i=1,#x do parts[#parts+1] = enc(x[i]) end
                return '['..table.concat(parts, ',')..']'
            else
                local keys = {}
                for k in pairs(x) do keys[#keys+1] = k end
                table.sort(keys, function(a,b) return tostring(a)<tostring(b) end)
                local parts = {}
                for _,k in ipairs(keys) do parts[#parts+1] = enc(k)..':'..enc(x[k]) end
                return '{'..table.concat(parts, ',')..'}'
            end
        else
            return 'null'
        end
    end
    print(enc(sort(v)))
end
function __min(v)
    local items
    if type(v) == 'table' and v.items ~= nil then
        items = v.items
    elseif type(v) == 'table' then
        items = v
    else
        error('min() expects list or group')
    end
    if #items == 0 then return 0 end
    local m = items[1]
    if type(m) == 'string' then
        for i=2,#items do
            local it = items[i]
            if type(it) == 'string' and it < m then m = it end
        end
        return m
    else
        m = tonumber(m)
        for i=2,#items do
            local n = tonumber(items[i])
            if n < m then m = n end
        end
        return m
    end
end
function __query(src, joins, opts)
    local whereFn = opts.where
    local items = {}
    if #joins == 0 and whereFn then
        for _, v in ipairs(src) do if whereFn(v) then items[#items+1] = {v} end end
    else
        for _, v in ipairs(src) do items[#items+1] = {v} end
    end
    for ji, j in ipairs(joins) do
        local joined = {}
        local jitems = j.items or {}
        if j.right and j.left then
            local matched = {}
            for _, left in ipairs(items) do
                local m = false
                for ri, right in ipairs(jitems) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true; matched[ri] = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local row = {table.unpack(left)}
                    row[#row+1] = nil
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
            for ri, right in ipairs(jitems) do
                if not matched[ri] then
                    local row = {}
                    for _=1,ji do row[#row+1] = nil end
                    row[#row+1] = right
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        elseif j.right then
            for _, right in ipairs(jitems) do
                local m = false
                for _, left in ipairs(items) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if not m then
                    local row = {}
                    for _=1,ji do row[#row+1] = nil end
                    row[#row+1] = right
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        else
            for _, left in ipairs(items) do
                local m = false
                for _, right in ipairs(jitems) do
                    local keep = true
                    if j.on then
                        local args = {table.unpack(left)}
                        args[#args+1] = right
                        keep = j.on(table.unpack(args))
                    end
                    if keep then
                        m = true
                        local row = {table.unpack(left)}
                        row[#row+1] = right
                        if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                        else
                            joined[#joined+1] = row
                        end
                    end
                end
                if j.left and not m then
                    local row = {table.unpack(left)}
                    row[#row+1] = nil
                    if ji == #joins and whereFn and not whereFn(table.unpack(row)) then
                    else
                        joined[#joined+1] = row
                    end
                end
            end
        end
        items = joined
    end
    if opts.sortKey then
        local pairs = {}
        for _, it in ipairs(items) do pairs[#pairs+1] = {item=it, key=opts.sortKey(table.unpack(it))} end
        table.sort(pairs, function(a,b)
            local ak, bk = a.key, b.key
            if type(ak)=='number' and type(bk)=='number' then return ak < bk end
            if type(ak)=='string' and type(bk)=='string' then return ak < bk end
            return tostring(ak) < tostring(bk)
        end)
        items = {}
        for i,p in ipairs(pairs) do items[i] = p.item end
    end
    if opts.skip ~= nil then
        local n = opts.skip
        if n < #items then
            for i=1,n do table.remove(items,1) end
        else
            items = {}
        end
    end
    if opts.take ~= nil then
        local n = opts.take
        if n < #items then
            for i=#items, n+1, -1 do table.remove(items) end
        end
    end
    local res = {}
    for _, r in ipairs(items) do res[#res+1] = opts.selectFn(table.unpack(r)) end
    return res
end
function __run_tests(tests)
    local function format_duration(d)
        if d < 1e-6 then return string.format('%dns', math.floor(d*1e9)) end
        if d < 1e-3 then return string.format('%.1fµs', d*1e6) end
        if d < 1 then return string.format('%.1fms', d*1e3) end
        return string.format('%.2fs', d)
    end
    local failures = 0
    for _, t in ipairs(tests) do
        io.write('   test ' .. t.name .. ' ...')
        local start = os.clock()
        local ok, err = pcall(t.fn)
        local dur = os.clock() - start
        if ok then
            io.write(' ok (' .. format_duration(dur) .. ')\n')
        else
            io.write(' fail ' .. tostring(err) .. ' (' .. format_duration(dur) .. ')\n')
            failures = failures + 1
        end
    end
    if failures > 0 then
        io.write('\n[FAIL] ' .. failures .. ' test(s) failed.\n')
    end
end
function test_Q19_finds_female_voice_actress_in_US_Japan_release_between_2005_and_2009()
    if not (__eq(result, {{["voicing_actress"]="Angela Stone", ["voiced_movie"]="Voiced Movie"}})) then error('expect failed') end
end

aka_name = {{["person_id"]=1, ["name"]="A. Stone"}, {["person_id"]=2, ["name"]="J. Doe"}}
char_name = {{["id"]=1, ["name"]="Protagonist"}, {["id"]=2, ["name"]="Extra"}}
cast_info = {{["movie_id"]=1, ["person_role_id"]=1, ["person_id"]=1, ["role_id"]=1, ["note"]="(voice)"}, {["movie_id"]=2, ["person_role_id"]=2, ["person_id"]=2, ["role_id"]=2, ["note"]="Cameo"}}
company_name = {{["id"]=10, ["country_code"]="[us]"}, {["id"]=20, ["country_code"]="[gb]"}}
info_type = {{["id"]=100, ["info"]="release dates"}}
movie_companies = {{["movie_id"]=1, ["company_id"]=10, ["note"]="Studio (USA)"}, {["movie_id"]=2, ["company_id"]=20, ["note"]="Other (worldwide)"}}
movie_info = {{["movie_id"]=1, ["info_type_id"]=100, ["info"]="USA: June 2006"}, {["movie_id"]=2, ["info_type_id"]=100, ["info"]="UK: 1999"}}
name = {{["id"]=1, ["name"]="Angela Stone", ["gender"]="f"}, {["id"]=2, ["name"]="Bob Angstrom", ["gender"]="m"}}
role_type = {{["id"]=1, ["role"]="actress"}, {["id"]=2, ["role"]="actor"}}
title = {{["id"]=1, ["title"]="Voiced Movie", ["production_year"]=2006}, {["id"]=2, ["title"]="Other Movie", ["production_year"]=2010}}
matches = (function()
    local _src = aka_name
    return __query(_src, {
        { items = name, on = function(an, n) return __eq(n.id, an.person_id) end },
        { items = cast_info, on = function(an, n, ci) return __eq(ci.person_id, an.person_id) end },
        { items = char_name, on = function(an, n, ci, chn) return __eq(chn.id, ci.person_role_id) end },
        { items = role_type, on = function(an, n, ci, chn, rt) return __eq(rt.id, ci.role_id) end },
        { items = title, on = function(an, n, ci, chn, rt, t) return __eq(t.id, ci.movie_id) end },
        { items = movie_companies, on = function(an, n, ci, chn, rt, t, mc) return __eq(mc.movie_id, t.id) end },
        { items = company_name, on = function(an, n, ci, chn, rt, t, mc, cn) return __eq(cn.id, mc.company_id) end },
        { items = movie_info, on = function(an, n, ci, chn, rt, t, mc, cn, mi) return __eq(mi.movie_id, t.id) end },
        { items = info_type, on = function(an, n, ci, chn, rt, t, mc, cn, mi, it) return __eq(it.id, mi.info_type_id) end }
    }, { selectFn = function(an, n, ci, chn, rt, t, mc, cn, mi, it) return {["actress"]=n.name, ["movie"]=t.title} end, where = function(an, n, ci, chn, rt, t, mc, cn, mi, it) return ((((((((((((__contains({"(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"}, ci.note) and __eq(cn.country_code, "[us]")) and __eq(it.info, "release dates")) and not __eq(mc.note, nil)) and ((__contains(mc.note, "(USA)") or __contains(mc.note, "(worldwide)")))) and not __eq(mi.info, nil)) and ((((__contains(mi.info, "Japan:") and __contains(mi.info, "200"))) or ((__contains(mi.info, "USA:") and __contains(mi.info, "200")))))) and __eq(n.gender, "f")) and __contains(n.name, "Ang")) and __eq(rt.role, "actress")) and (t.production_year >= 2005)) and (t.production_year <= 2009))) end })
end)()
result = {{["voicing_actress"]=__min((function()
    local _res = {}
    for _, r in ipairs(matches) do
        _res[#_res+1] = r.actress
    end
    return _res
end)()), ["voiced_movie"]=__min((function()
    local _res = {}
    for _, r in ipairs(matches) do
        _res[#_res+1] = r.movie
    end
    return _res
end)())}}
__json(result)
local __tests = {
    {name="Q19 finds female voice actress in US/Japan release between 2005 and 2009", fn=test_Q19_finds_female_voice_actress_in_US_Japan_release_between_2005_and_2009},
}
