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

local function index(obj, i)
	if type(obj)=='string' then
		local len=#obj
		if i<0 then i=len+i+1 else i=i+1 end
		return string.sub(obj,i,i)
	elseif type(obj)=='table' then
		return obj[i+1]
	else
		return nil
	end
end

local s = "mochi"
print_value(index(s, 1))
