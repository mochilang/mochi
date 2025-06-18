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

function reverse(x)
	local sign = 1
	local n = x
	if (n < 0) then
		sign = -1
		n = -n
	end
	local rev = 0
	while not __eq(n, 0) do
		local digit = (n % 10)
		rev = __add((rev * 10), digit)
		n = __div(n, 10)
		::__continue0::
	end
	rev = (rev * sign)
	if ((rev < ((-2147483647 - 1))) or (rev > 2147483647)) then
		return 0
	end
	return rev
end

function test_example_1()
	if not (__eq(reverse(123), 321)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(reverse(-123), (-321))) then error('expect failed') end
end

function test_example_3()
	if not (__eq(reverse(120), 21)) then error('expect failed') end
end

function test_overflow()
	if not (__eq(reverse(1534236469), 0)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_overflow()
