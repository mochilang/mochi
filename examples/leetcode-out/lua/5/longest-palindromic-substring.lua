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

function expand(s, left, right)
	local l = left
	local r = right
	local n = #s
	while ((l >= 0) and (r < n)) do
		if not __eq(__index(s, l), __index(s, r)) then
			break
		end
		l = (l - 1)
		r = __add(r, 1)
		::__continue0::
	end
	return ((r - l) - 1)
end

function longestPalindrome(s)
	if (#s <= 1) then
		return s
	end
	local start = 0
	local _end = 0
	local n = #s
	for i = 0, (n)-1 do
		local len1 = expand(s, i, i)
		local len2 = expand(s, i, __add(i, 1))
		local l = len1
		if (len2 > len1) then
			l = len2
		end
		if (l > (_end - start)) then
			start = (i - __div(((l - 1)), 2))
			_end = __add(i, __div(l, 2))
		end
		::__continue1::
	end
	return __slice(s, start, __add(_end, 1))
end

function test_example_1()
	local ans = longestPalindrome("babad")
	if not ((__eq(ans, "bab") or __eq(ans, "aba"))) then error('expect failed') end
end

function test_example_2()
	if not (__eq(longestPalindrome("cbbd"), "bb")) then error('expect failed') end
end

function test_single_char()
	if not (__eq(longestPalindrome("a"), "a")) then error('expect failed') end
end

function test_two_chars()
	local ans = longestPalindrome("ac")
	if not ((__eq(ans, "a") or __eq(ans, "c"))) then error('expect failed') end
end

test_example_1()
test_example_2()
test_single_char()
test_two_chars()
