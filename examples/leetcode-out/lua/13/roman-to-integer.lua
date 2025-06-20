function __add(a, b)
	if type(a) == 'table' and type(b) == 'table' then
		local out = {}
		for i = 1, #a do out[#out+1] = a[i] end
		for i = 1, #b do out[#out+1] = b[i] end
		return out
	elseif type(a) == 'string' or type(b) == 'string' then
		return tostring(a) .. tostring(b)
	else
		return a + b
	end
end

function __eq(a, b)
	if type(a) ~= type(b) then return false end
	if type(a) ~= 'table' then return a == b end
	if (a[1] ~= nil or #a > 0) and (b[1] ~= nil or #b > 0) then
		if #a ~= #b then return false end
		for i = 1, #a do if not __eq(a[i], b[i]) then return false end end
		return true
	end
	for k, v in pairs(a) do if not __eq(v, b[k]) then return false end end
	for k, _ in pairs(b) do if a[k] == nil then return false end end
	return true
end

function __index(obj, i)
	if type(obj) == 'string' then
		return __indexString(obj, i)
	elseif type(obj) == 'table' then
		if obj[1] ~= nil or #obj > 0 then
			return obj[(i)+1]
		else
			return obj[i]
		end
	else
		error('cannot index')
	end
end

function __indexString(s, i)
	local len = #s
	if i < 0 then
		i = len + i + 1
	else
		i = i + 1
	end
	if i < 1 or i > len then error('index out of range') end
	return string.sub(s, i, i)
end

function romanToInt(s)
	local values = {["I"]=1, ["V"]=5, ["X"]=10, ["L"]=50, ["C"]=100, ["D"]=500, ["M"]=1000}
	local total = 0
	local i = 0
	local n = #s
	while (i < n) do
		local curr = __index(values, __index(s, i))
		if (__add(i, 1) < n) then
			local next = __index(values, __index(s, __add(i, 1)))
			if (curr < next) then
				total = (__add(total, next) - curr)
				i = __add(i, 2)
				goto __continue0
			end
		end
		total = __add(total, curr)
		i = __add(i, 1)
		::__continue0::
	end
	return total
end

function test_example_1()
	if not (__eq(romanToInt("III"), 3)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(romanToInt("LVIII"), 58)) then error('expect failed') end
end

function test_example_3()
	if not (__eq(romanToInt("MCMXCIV"), 1994)) then error('expect failed') end
end

function test_subtractive()
	if not (__eq(romanToInt("IV"), 4)) then error('expect failed') end
	if not (__eq(romanToInt("IX"), 9)) then error('expect failed') end
end

function test_tens()
	if not (__eq(romanToInt("XL"), 40)) then error('expect failed') end
	if not (__eq(romanToInt("XC"), 90)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_subtractive()
test_tens()
