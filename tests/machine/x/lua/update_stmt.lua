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
Person = {}
Person.__index = Person
function Person.new(o)
  o = o or {}
  setmetatable(o, Person)
  return o
end

function test_update_adult_status()
  if not (__eq(people, {{name="Alice", age=17, status="minor"}, {name="Bob", age=26, status="adult"}, {name="Charlie", age=19, status="adult"}, {name="Diana", age=16, status="minor"}})) then error('expect failed') end
end

people = {{name="Alice", age=17, status="minor"}, {name="Bob", age=25, status="unknown"}, {name="Charlie", age=18, status="unknown"}, {name="Diana", age=16, status="minor"}}
for _i0 = 1, #people do
  local _it0 = people[_i0]
  local name = _it0["name"]
  local age = _it0["age"]
  local status = _it0["status"]
  if (age >= 18) then
    _it0["status"] = "adult"
    _it0["age"] = __add(age, 1)
  end
  people[_i0] = _it0
end
print("ok")
local __tests = {
  {name="update adult status", fn=test_update_adult_status},
}
