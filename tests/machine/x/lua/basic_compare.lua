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

local a = (10 - 3)
local b = (2 + 2)
print_value(a)
print_value((a == 7))
print_value((b < 5))
