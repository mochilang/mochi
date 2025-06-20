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

function strStr(haystack, needle)
	local n = #haystack
	local m = #needle
	if __eq(m, 0) then
		return 0
	end
	if (m > n) then
		return -1
	end
	for i = 0, (__add((n - m), 1))-1 do
		local j = 0
		while (j < m) do
			if not __eq(__index(haystack, __add(i, j)), __index(needle, j)) then
				break
			end
			j = __add(j, 1)
			::__continue1::
		end
		if __eq(j, m) then
			return i
		end
		::__continue0::
	end
	return -1
end

function test_example_1()
	if not (__eq(strStr("sadbutsad", "sad"), 0)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(strStr("leetcode", "leeto"), (-1))) then error('expect failed') end
end

function test_empty_needle()
	if not (__eq(strStr("abc", ""), 0)) then error('expect failed') end
end

function test_needle_at_end()
	if not (__eq(strStr("hello", "lo"), 3)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_empty_needle()
test_needle_at_end()
