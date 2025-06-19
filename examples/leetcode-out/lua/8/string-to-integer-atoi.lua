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

function __slice(obj, i, j)
	if i == nil then i = 0 end
	if type(obj) == 'string' then
		local len = #obj
		if j == nil then j = len end
		if i < 0 then i = len + i end
		if j < 0 then j = len + j end
		if i < 0 then i = 0 end
		if j > len then j = len end
		return string.sub(obj, i+1, j)
	elseif type(obj) == 'table' then
		local len = #obj
		if j == nil then j = len end
		if i < 0 then i = len + i end
		if j < 0 then j = len + j end
		if i < 0 then i = 0 end
		if j > len then j = len end
		local out = {}
		for k = i+1, j do
			out[#out+1] = obj[k]
		end
		return out
	else
		return {}
	end
end

function digit(ch)
	if __eq(ch, "0") then
		return 0
	end
	if __eq(ch, "1") then
		return 1
	end
	if __eq(ch, "2") then
		return 2
	end
	if __eq(ch, "3") then
		return 3
	end
	if __eq(ch, "4") then
		return 4
	end
	if __eq(ch, "5") then
		return 5
	end
	if __eq(ch, "6") then
		return 6
	end
	if __eq(ch, "7") then
		return 7
	end
	if __eq(ch, "8") then
		return 8
	end
	if __eq(ch, "9") then
		return 9
	end
	return -1
end

function myAtoi(s)
	local i = 0
	local n = #s
	while ((i < n) and __eq(__index(s, i), __index(" ", 0))) do
		i = __add(i, 1)
		::__continue0::
	end
	local sign = 1
	if ((i < n) and ((__eq(__index(s, i), __index("+", 0)) or __eq(__index(s, i), __index("-", 0))))) then
		if __eq(__index(s, i), __index("-", 0)) then
			sign = -1
		end
		i = __add(i, 1)
	end
	local result = 0
	while (i < n) do
		local ch = __slice(s, i, __add(i, 1))
		local d = digit(ch)
		if (d < 0) then
			break
		end
		result = __add((result * 10), d)
		i = __add(i, 1)
		::__continue1::
	end
	result = (result * sign)
	if (result > 2147483647) then
		return 2147483647
	end
	if (result < (-2147483648)) then
		return -2147483648
	end
	return result
end

function test_example_1()
	if not (__eq(myAtoi("42"), 42)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(myAtoi("   -42"), (-42))) then error('expect failed') end
end

function test_example_3()
	if not (__eq(myAtoi("4193 with words"), 4193)) then error('expect failed') end
end

function test_example_4()
	if not (__eq(myAtoi("words and 987"), 0)) then error('expect failed') end
end

function test_example_5()
	if not (__eq(myAtoi("-91283472332"), (-2147483648))) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_example_4()
test_example_5()
