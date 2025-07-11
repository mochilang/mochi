local math = { sqrt = math.sqrt, pow = math.pow, sin = math.sin, log = math.log, pi = math.pi, e = math.exp(1) }
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)(math.sqrt(16.0))
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)(math.pi)
