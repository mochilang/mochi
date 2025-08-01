-- Generated by Mochi v0.10.50 on 2025-07-30 21:21 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local _now_seed = 0
local _now_seeded = false
do
  local s = os.getenv("MOCHI_NOW_SEED")
  if s and s ~= "" then
    local v = tonumber(s)
    if v then
      _now_seed = v
      _now_seeded = true
    end
  end
end
local function _now()
if _now_seeded then
  -- keep the seed within safe integer range for Lua (53 bits)
  _now_seed = (_now_seed * 1664525 + 1013904223) % 9007199254740991
  return _now_seed % 1000000000
end
return os.time() * 1000000000 + math.floor(os.clock() * 1000000000)
end
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function makeNode(n)
    return {val = {num = n, denom = 1}, txt = tostring(n)}
  end
  function combine(op, l, r)
    local res = nil
    if (op == OP_ADD) then
      res = {num = ((tonumber((l.val.num * r.val.denom)) or 0) + (tonumber((l.val.denom * r.val.num)) or 0)), denom = (l.val.denom * r.val.denom)}
    else
      if (op == OP_SUB) then
        res = {num = ((l.val.num * r.val.denom) - (l.val.denom * r.val.num)), denom = (l.val.denom * r.val.denom)}
      else
        if (op == OP_MUL) then
          res = {num = (l.val.num * r.val.num), denom = (l.val.denom * r.val.denom)}
        else
          res = {num = (l.val.num * r.val.denom), denom = (l.val.denom * r.val.num)}
        end
      end
    end
    local opstr = ""
    if (op == OP_ADD) then
      opstr = " + "
    else
      if (op == OP_SUB) then
        opstr = " - "
      else
        if (op == OP_MUL) then
          opstr = " * "
        else
          opstr = " / "
        end
      end
    end
    return {val = res, txt = (((("(" .. tostring(l.txt)) .. opstr) .. tostring(r.txt)) .. ")")}
  end
  function exprEval(x)
    return x.val
  end
  function exprString(x)
    return x.txt
  end
  function solve(xs)
    if ((function(v)
    if type(v) == 'table' and v.items ~= nil then
      return #v.items
    elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0
        for _ in pairs(v) do c = c + 1 end
        return c
      elseif type(v) == 'string' then
          local l = utf8.len(v)
          if l then return l end
          return #v
        elseif type(v) == 'table' then
            return #v
          else
            return 0
          end
        end)(xs) == 1) then
          local f = exprEval(xs[0 + 1])
          if ((f.denom ~= 0) and (f.num == (f.denom * goal))) then
            print(exprString(xs[0 + 1]))
            return true
          end
          return false
        end
        local i = 0
        while (i < (function(v)
        if type(v) == 'table' and v.items ~= nil then
          return #v.items
        elseif type(v) == 'table' and (v[1] == nil) then
            local c = 0
            for _ in pairs(v) do c = c + 1 end
            return c
          elseif type(v) == 'string' then
              local l = utf8.len(v)
              if l then return l end
              return #v
            elseif type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(xs)) do
              local j = (i + 1)
              while (j < (function(v)
              if type(v) == 'table' and v.items ~= nil then
                return #v.items
              elseif type(v) == 'table' and (v[1] == nil) then
                  local c = 0
                  for _ in pairs(v) do c = c + 1 end
                  return c
                elseif type(v) == 'string' then
                    local l = utf8.len(v)
                    if l then return l end
                    return #v
                  elseif type(v) == 'table' then
                      return #v
                    else
                      return 0
                    end
                  end)(xs)) do
                    local rest = {}
                    local k = 0
                    while (k < (function(v)
                    if type(v) == 'table' and v.items ~= nil then
                      return #v.items
                    elseif type(v) == 'table' and (v[1] == nil) then
                        local c = 0
                        for _ in pairs(v) do c = c + 1 end
                        return c
                      elseif type(v) == 'string' then
                          local l = utf8.len(v)
                          if l then return l end
                          return #v
                        elseif type(v) == 'table' then
                            return #v
                          else
                            return 0
                          end
                        end)(xs)) do
                          if ((k ~= i) and (k ~= j)) then
                            rest = (function(lst, item)
                            local res = {table.unpack(lst)}
                            table.insert(res, item)
                            return res
                          end)(rest, xs[k + 1])
                        end
                        k = (k + 1)
                      end
                      local a = xs[i + 1]
                      local b = xs[j + 1]
                      local node = nil
                      for _, op in ipairs({OP_ADD, OP_SUB, OP_MUL, OP_DIV}) do
                        node = combine(op, (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(a), (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(b))
                        if solve((function(lst, item)
                        local res = {table.unpack(lst)}
                        table.insert(res, item)
                        return res
                      end)(rest, node)) then
                        return true
                      end
                    end
                    node = combine(OP_SUB, (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(b), (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(a))
                    if solve((function(lst, item)
                    local res = {table.unpack(lst)}
                    table.insert(res, item)
                    return res
                  end)(rest, node)) then
                    return true
                  end
                  node = combine(OP_DIV, (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(b), (function(_t) local _c={} for k,v in pairs(_t) do _c[k]=v end return _c end)(a))
                  if solve((function(lst, item)
                  local res = {table.unpack(lst)}
                  table.insert(res, item)
                  return res
                end)(rest, node)) then
                  return true
                end
                j = (j + 1)
              end
              i = (i + 1)
            end
            return false
          end
          function main()
            local iter = 0
            while (iter < 10) do
              local cards = {}
              local i = 0
              while (i < n_cards) do
                local n = ((_now() % (digit_range - 1)) + 1)
                cards = (function(lst, item)
                local res = {table.unpack(lst)}
                table.insert(res, item)
                return res
              end)(cards, makeNode(n))
              print((" " .. tostring(n)))
              i = (i + 1)
            end
            print(":  ")
            if (not solve(cards)) then
              print("No solution")
            end
            iter = (iter + 1)
          end
        end
        OP_ADD = 1
        OP_SUB = 2
        OP_MUL = 3
        OP_DIV = 4
        n_cards = 4
        goal = 24
        digit_range = 9
        main()
        local _bench_end = _now()
        collectgarbage()
        local _bench_end_mem = collectgarbage('count') * 1024
        local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
        local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
        print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
      end;
