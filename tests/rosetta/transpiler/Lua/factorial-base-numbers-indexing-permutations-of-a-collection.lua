-- Generated by Mochi v0.10.42 on 2025-07-27 16:50 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local function _parseIntStr(str)
if type(str) == 'table' then
  str = table.concat(str)
end
local n = tonumber(str, 10)
if n == nil then return 0 end
return math.floor(n)
end

local function _split(s, sep)
local t = {}
local pattern = string.format("([^%s]+)", sep)
string.gsub(s, pattern, function(c) t[#t+1] = c end)
return t
end

local function slice(lst, s, e)
if s < 0 then s = #lst + s end
if e == nil then e = #lst end
local r = {}
for i = s + 1, e do
  r[#r+1] = lst[i]
end
return r
end
function split(s, sep)
  local parts = {}
  local cur = ""
  local i = 0
  while (i < (function(v)
  if type(v) == 'table' and v.items ~= nil then
    return #v.items
  elseif type(v) == 'table' and (v[1] == nil) then
      local c = 0
      for _ in pairs(v) do c = c + 1 end
      return c
    elseif type(v) == 'string' or type(v) == 'table' then
        return #v
      else
        return 0
      end
    end)(s)) do
      if ((((function(v)
      if type(v) == 'table' and v.items ~= nil then
        return #v.items
      elseif type(v) == 'table' and (v[1] == nil) then
          local c = 0
          for _ in pairs(v) do c = c + 1 end
          return c
        elseif type(v) == 'string' or type(v) == 'table' then
            return #v
          else
            return 0
          end
        end)(sep) > 0) and ((i + (function(v)
        if type(v) == 'table' and v.items ~= nil then
          return #v.items
        elseif type(v) == 'table' and (v[1] == nil) then
            local c = 0
            for _ in pairs(v) do c = c + 1 end
            return c
          elseif type(v) == 'string' or type(v) == 'table' then
              return #v
            else
              return 0
            end
          end)(sep)) <= (function(v)
          if type(v) == 'table' and v.items ~= nil then
            return #v.items
          elseif type(v) == 'table' and (v[1] == nil) then
              local c = 0
              for _ in pairs(v) do c = c + 1 end
              return c
            elseif type(v) == 'string' or type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(s))) and (string.sub(s, i + 1, (i + (function(v)
            if type(v) == 'table' and v.items ~= nil then
              return #v.items
            elseif type(v) == 'table' and (v[1] == nil) then
                local c = 0
                for _ in pairs(v) do c = c + 1 end
                return c
              elseif type(v) == 'string' or type(v) == 'table' then
                  return #v
                else
                  return 0
                end
              end)(sep))) == sep)) then
                parts = (function(lst, item)
                local res = {table.unpack(lst)}
                table.insert(res, item)
                return res
              end)(parts, cur)
              cur = ""
              i = (i + (function(v)
              if type(v) == 'table' and v.items ~= nil then
                return #v.items
              elseif type(v) == 'table' and (v[1] == nil) then
                  local c = 0
                  for _ in pairs(v) do c = c + 1 end
                  return c
                elseif type(v) == 'string' or type(v) == 'table' then
                    return #v
                  else
                    return 0
                  end
                end)(sep))
              else
                cur = (cur .. string.sub(s, i + 1, (i + 1)))
                i = (i + 1)
              end
            end
            parts = (function(lst, item)
            local res = {table.unpack(lst)}
            table.insert(res, item)
            return res
          end)(parts, cur)
          return parts
        end;
        
        function parseIntStr(str)
          local i = 0
          local neg = false
          if (((function(v)
          if type(v) == 'table' and v.items ~= nil then
            return #v.items
          elseif type(v) == 'table' and (v[1] == nil) then
              local c = 0
              for _ in pairs(v) do c = c + 1 end
              return c
            elseif type(v) == 'string' or type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(str) > 0) and (string.sub(str, (0 + 1), 1) == "-")) then
              neg = true
              i = 1
            end
            local n = 0
            local digits = {__name = "GenType1", __order = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}, ["0"] = 0, ["1"] = 1, ["2"] = 2, ["3"] = 3, ["4"] = 4, ["5"] = 5, ["6"] = 6, ["7"] = 7, ["8"] = 8, ["9"] = 9}
            while (i < (function(v)
            if type(v) == 'table' and v.items ~= nil then
              return #v.items
            elseif type(v) == 'table' and (v[1] == nil) then
                local c = 0
                for _ in pairs(v) do c = c + 1 end
                return c
              elseif type(v) == 'string' or type(v) == 'table' then
                  return #v
                else
                  return 0
                end
              end)(str)) do
                n = ((n * 10) + digits[string.sub(str, (i + 1), (i + 1))])
                i = (i + 1)
              end
              if neg then
                n = (-n)
              end
              return n
            end;
            
            function joinInts(nums, sep)
              local s = ""
              local i = 0
              while (i < (function(v)
              if type(v) == 'table' and v.items ~= nil then
                return #v.items
              elseif type(v) == 'table' and (v[1] == nil) then
                  local c = 0
                  for _ in pairs(v) do c = c + 1 end
                  return c
                elseif type(v) == 'string' or type(v) == 'table' then
                    return #v
                  else
                    return 0
                  end
                end)(nums)) do
                  if (i > 0) then
                    s = (s .. sep)
                  end
                  s = (s .. tostring(nums[i + 1]))
                  i = (i + 1)
                end
                return s
              end;
              
              function undot(s)
                local parts = _split(s, ".")
                local nums = {}
                for _, p in ipairs(parts) do
                  nums = (function(lst, item)
                  local res = {table.unpack(lst)}
                  table.insert(res, item)
                  return res
                end)(nums, _parseIntStr(p))
              end
              return nums
            end;
            
            function factorial(n)
              local f = 1
              local i = 2
              while (i <= n) do
                f = (f * i)
                i = (i + 1)
              end
              return f
            end;
            
            function genFactBaseNums(size, countOnly)
              local results = {}
              local count = 0
              local n = 0
              while true do
                local radix = 2
                local res = {}
                if (not countOnly) then
                  local z = 0
                  while (z < size) do
                    res = (function(lst, item)
                    local res = {table.unpack(lst)}
                    table.insert(res, item)
                    return res
                  end)(res, 0)
                  z = (z + 1)
                end
              end
              local k = n
              while (k > 0) do
                local div = (k // radix)
                local rem = (k % radix)
                if ((not countOnly) and (radix <= (size + 1))) then
                  res[((size - radix) + 1) + 1] = rem
                end
                k = div
                radix = (radix + 1)
              end
              if (radix > (size + 2)) then
                break
              end
              count = (count + 1)
              if (not countOnly) then
                results = (function(lst, item)
                local res = {table.unpack(lst)}
                table.insert(res, item)
                return res
              end)(results, res)
            end
            n = (n + 1)
          end
          return {results, count}
        end;
        
        function mapToPerms(factNums)
          local perms = {}
          local psize = ((function(v)
          if type(v) == 'table' and v.items ~= nil then
            return #v.items
          elseif type(v) == 'table' and (v[1] == nil) then
              local c = 0
              for _ in pairs(v) do c = c + 1 end
              return c
            elseif type(v) == 'string' or type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(factNums[0 + 1]) + 1)
            local start = {}
            local i = 0
            while (i < psize) do
              start = (function(lst, item)
              local res = {table.unpack(lst)}
              table.insert(res, item)
              return res
            end)(start, i)
            i = (i + 1)
          end
          for _, fn in ipairs(factNums) do
            local perm = {}
            local j = 0
            while (j < (function(v)
            if type(v) == 'table' and v.items ~= nil then
              return #v.items
            elseif type(v) == 'table' and (v[1] == nil) then
                local c = 0
                for _ in pairs(v) do c = c + 1 end
                return c
              elseif type(v) == 'string' or type(v) == 'table' then
                  return #v
                else
                  return 0
                end
              end)(start)) do
                perm = (function(lst, item)
                local res = {table.unpack(lst)}
                table.insert(res, item)
                return res
              end)(perm, start[j + 1])
              j = (j + 1)
            end
            local m = 0
            while (m < (function(v)
            if type(v) == 'table' and v.items ~= nil then
              return #v.items
            elseif type(v) == 'table' and (v[1] == nil) then
                local c = 0
                for _ in pairs(v) do c = c + 1 end
                return c
              elseif type(v) == 'string' or type(v) == 'table' then
                  return #v
                else
                  return 0
                end
              end)(fn)) do
                local g = fn[m + 1]
                if (g ~= 0) then
                  local first = m
                  local last = ((tonumber(m) or 0) + (tonumber(g) or 0))
                  local t = 1
                  while (t <= g) do
                    local temp = perm[first + 1]
                    local x = (first + 1)
                    while (x <= last) do
                      perm[(x - 1) + 1] = perm[x + 1]
                      x = (x + 1)
                    end
                    perm[last + 1] = temp
                    t = (t + 1)
                  end
                end
                m = (m + 1)
              end
              perms = (function(lst, item)
              local res = {table.unpack(lst)}
              table.insert(res, item)
              return res
            end)(perms, perm)
          end
          return perms
        end;
        
        function randInt(n)
          seed = (((seed * 1664525) + 1013904223) % 2147483647)
          return (seed % n)
        end;
        
        function main()
          local g = genFactBaseNums(3, false)
          local factNums = g[0 + 1]
          local perms = mapToPerms(factNums)
          local i = 0
          while (i < (function(v)
          if type(v) == 'table' and v.items ~= nil then
            return #v.items
          elseif type(v) == 'table' and (v[1] == nil) then
              local c = 0
              for _ in pairs(v) do c = c + 1 end
              return c
            elseif type(v) == 'string' or type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(factNums)) do
              print(((joinInts(factNums[i + 1], ".") .. " -> ") .. joinInts(perms[i + 1], "")))
              i = (i + 1)
            end
            local count2 = factorial(11)
            print(("\nPermutations generated = " .. tostring(count2)))
            print(("compared to 11! which  = " .. tostring(factorial(11))))
            print("")
            local fbn51s = {"39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0", "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"}
            factNums = {undot(fbn51s[0 + 1]), undot(fbn51s[1 + 1])}
            perms = mapToPerms(factNums)
            local shoe = "A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣"
            local cards = {}
            i = 0
            while (i < 52) do
              local card = string.sub(shoe, (2 * i) + 1, ((2 * i) + 2))
              if (string.sub(card, (0 + 1), 1) == "T") then
                card = ("10" .. string.sub(card, (1 + 1), 2))
              end
              cards = (function(lst, item)
              local res = {table.unpack(lst)}
              table.insert(res, item)
              return res
            end)(cards, card)
            i = (i + 1)
          end
          i = 0
          while (i < (function(v)
          if type(v) == 'table' and v.items ~= nil then
            return #v.items
          elseif type(v) == 'table' and (v[1] == nil) then
              local c = 0
              for _ in pairs(v) do c = c + 1 end
              return c
            elseif type(v) == 'string' or type(v) == 'table' then
                return #v
              else
                return 0
              end
            end)(fbn51s)) do
              print(fbn51s[i + 1])
              local perm = perms[i + 1]
              local j = 0
              local line = ""
              while (j < (function(v)
              if type(v) == 'table' and v.items ~= nil then
                return #v.items
              elseif type(v) == 'table' and (v[1] == nil) then
                  local c = 0
                  for _ in pairs(v) do c = c + 1 end
                  return c
                elseif type(v) == 'string' or type(v) == 'table' then
                    return #v
                  else
                    return 0
                  end
                end)(perm)) do
                  line = (line .. cards[perm[j + 1] + 1])
                  j = (j + 1)
                end
                print((line .. "\n"))
                i = (i + 1)
              end
              local fbn51 = {}
              i = 0
              while (i < 51) do
                fbn51 = (function(lst, item)
                local res = {table.unpack(lst)}
                table.insert(res, item)
                return res
              end)(fbn51, randInt((52 - i)))
              i = (i + 1)
            end
            print(joinInts(fbn51, "."))
            perms = mapToPerms({fbn51})
            local line = ""
            i = 0
            while (i < (function(v)
            if type(v) == 'table' and v.items ~= nil then
              return #v.items
            elseif type(v) == 'table' and (v[1] == nil) then
                local c = 0
                for _ in pairs(v) do c = c + 1 end
                return c
              elseif type(v) == 'string' or type(v) == 'table' then
                  return #v
                else
                  return 0
                end
              end)(perms[0 + 1])) do
                line = (line .. cards[perms[0 + 1][i + 1] + 1])
                i = (i + 1)
              end
              print(line)
            end;
            
            seed = 1;
            
            main();
