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

function intToRoman(num)
	local values = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1}
	local symbols = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"}
	local result = ""
	local i = 0
	while (num > 0) do
		while (num >= __index(values, i)) do
			result = __add(result, __index(symbols, i))
			num = (num - __index(values, i))
			::__continue1::
		end
		i = __add(i, 1)
		::__continue0::
	end
	return result
end

function test_example_1()
	if not (__eq(intToRoman(3), "III")) then error('expect failed') end
end

function test_example_2()
	if not (__eq(intToRoman(58), "LVIII")) then error('expect failed') end
end

function test_example_3()
	if not (__eq(intToRoman(1994), "MCMXCIV")) then error('expect failed') end
end

function test_small_numbers()
	if not (__eq(intToRoman(4), "IV")) then error('expect failed') end
	if not (__eq(intToRoman(9), "IX")) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_small_numbers()
