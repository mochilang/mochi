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

function __contains(container, item)
	if type(container) == 'table' then
		if container[1] ~= nil or #container > 0 then
			for _, v in ipairs(container) do
				if v == item then return true end
			end
			return false
		else
			return container[item] ~= nil
		end
	elseif type(container) == 'string' then
		return string.find(container, item, 1, true) ~= nil
	else
		return false
	end
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

function isMatch(s, p)
	local m = #s
	local n = #p
	local memo = {}
	local function dfs(i, j)
		local key = __add((i * (__add(n, 1))), j)
		if __contains(memo, key) then
			return __index(memo, key)
		end
		if __eq(j, n) then
			return __eq(i, m)
		end
		local first = false
		if (i < m) then
			if ((__eq(__index(p, j), __index(s, i))) or (__eq(__index(p, j), "."))) then
				first = true
			end
		end
		local ans = false
		if (__add(j, 1) < n) then
			if __eq(__index(p, __add(j, 1)), "*") then
				if dfs(i, __add(j, 2)) then
					ans = true
				elseif (first and dfs(__add(i, 1), j)) then
					ans = true
				end
			else
				if (first and dfs(__add(i, 1), __add(j, 1))) then
					ans = true
				end
			end
		else
			if (first and dfs(__add(i, 1), __add(j, 1))) then
				ans = true
			end
		end
		memo[(key)+1] = ans
		return ans
	end
	return dfs(0, 0)
end

function test_example_1()
	if not (__eq(isMatch("aa", "a"), false)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(isMatch("aa", "a*"), true)) then error('expect failed') end
end

function test_example_3()
	if not (__eq(isMatch("ab", ".*"), true)) then error('expect failed') end
end

function test_example_4()
	if not (__eq(isMatch("aab", "c*a*b"), true)) then error('expect failed') end
end

function test_example_5()
	if not (__eq(isMatch("mississippi", "mis*is*p*."), false)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
test_example_4()
test_example_5()
