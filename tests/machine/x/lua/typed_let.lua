y = nil
;(function(...) local parts={} for i=1,select('#', ...) do local a=select(i, ...) if a~=nil and a~='' then parts[#parts+1]=tostring(a) end end print(table.concat(parts, ' ')) end)("<nil>")
