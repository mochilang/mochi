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

for i=1,(4)-1 do
	print_value(i)
end
