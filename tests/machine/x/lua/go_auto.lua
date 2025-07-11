local testpkg = { Add = function(a,b) return a + b end, Pi = 3.14, Answer = 42 }
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)(testpkg.Add(2, 3))
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)(testpkg.Pi)
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)(testpkg.Answer)
