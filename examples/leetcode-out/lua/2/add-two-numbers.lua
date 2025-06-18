function __div(a, b)
	if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
		return a // b
	end
	return a / b
end

function __add(a, b)
	if type(a) == 'table' and type(b) == 'table' then
		local out = {}
		for i = 1, #a do out[#out+1] = a[i] end
		for i = 1, #b do out[#out+1] = b[i] end
		return out
	end
	return a + b
end

function __eq(a, b)
	if type(a) == 'table' and type(b) == 'table' then
		if #a ~= #b then return false end
		for i = 1, #a do
			if not __eq(a[i], b[i]) then return false end
		end
		return true
	end
	return a == b
end

function __index(obj, i)
	if type(obj) == 'string' then
		return __indexString(obj, i)
	elseif type(obj) == 'table' then
		return obj[(i)+1]
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

function addTwoNumbers(l1, l2)
	local i = 0
	local j = 0
	local carry = 0
	local result = {}
	while (((i < #l1) or (j < #l2)) or (carry > 0)) do
		local x = 0
		if (i < #l1) then
			x = __index(l1, i)
			i = __add(i, 1)
		end
		local y = 0
		if (j < #l2) then
			y = __index(l2, j)
			j = __add(j, 1)
		end
		local sum = __add(__add(x, y), carry)
		local digit = (sum % 10)
		carry = __div(sum, 10)
		result = __add(result, {digit})
		::__continue0::
	end
	return result
end

function test_example_1()
	if not (__eq(addTwoNumbers({2, 4, 3}, {5, 6, 4}), {7, 0, 8})) then error('expect failed') end
end

function test_example_2()
	if not (__eq(addTwoNumbers({0}, {0}), {0})) then error('expect failed') end
end

function test_example_3()
	if not (__eq(addTwoNumbers({9, 9, 9, 9, 9, 9, 9}, {9, 9, 9, 9}), {8, 9, 9, 9, 0, 0, 0, 1})) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
