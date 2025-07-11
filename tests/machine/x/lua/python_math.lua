function __div(a, b)
    if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
        return a // b
    end
    return a / b
end
local math = { sqrt = math.sqrt, pow = math.pow, sin = math.sin, log = math.log, pi = math.pi, e = math.exp(1) }
r = 3.0
area = (math.pi * math.pow(r, 2.0))
root = math.sqrt(49.0)
sin45 = math.sin(__div(math.pi, 4.0))
log_e = math.log(math.e)
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)("Circle area with r =", r, "=>", area)
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)("Square root of 49:", root)
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)("sin(π/4):", sin45)
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)("log(e):", log_e)
