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

local function boom()
	print_value("boom")
	return true
end
print_value(((((1 < 2)) and ((2 < 3))) and ((3 < 4))))
print_value(((((1 < 2)) and ((2 > 3))) and boom()))
print_value((((((1 < 2)) and ((2 < 3))) and ((3 > 4))) and boom()))
