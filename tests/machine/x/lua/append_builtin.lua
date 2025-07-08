local function append(lst, v)
	local out = {}
	for i=1,#lst do out[#out+1]=lst[i] end
	out[#out+1]=v
	return out
end

local function print_value(v)
	if v == nil then
		print('<nil>')
		return
	end
	if type(v)=='number' and v == math.floor(v) then
		print(math.floor(v))
		return
	end
	if type(v)=='table' then
		for i,x in ipairs(v) do
			io.write(x)
			if i < #v then io.write(' ') end
		end
		io.write('\n')
	else
		print(v)
	end
end

local a = {1, 2}
print_value(append(a, 3))
