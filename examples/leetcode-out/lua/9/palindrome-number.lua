function __div(a, b)
	if math.type and math.type(a) == 'integer' and math.type(b) == 'integer' then
		return a // b
	end
	return a / b
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

function isPalindrome(x)
	if (x < 0) then
		return false
	end
	local s = tostring(x)
	local n = #s
	for i = 0, (__div(n, 2))-1 do
		if not __eq(__index(s, i), __index(s, ((n - 1) - i))) then
			return false
		end
		::__continue0::
	end
	return true
end

function test_example_1()
	if not (__eq(isPalindrome(121), true)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(isPalindrome(-121), false)) then error('expect failed') end
end

function test_example_3()
	if not (__eq(isPalindrome(10), false)) then error('expect failed') end
end

function test_zero()
	if not (__eq(isPalindrome(0), true)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_zero()
