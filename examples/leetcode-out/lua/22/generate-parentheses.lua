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

function generateParenthesis(n)
	local result = {}
	local function backtrack(current, open, close)
		if __eq(#current, (n * 2)) then
			result = __add(result, {current})
		else
			if (open < n) then
				backtrack(__add(current, "("), __add(open, 1), close)
			end
			if (close < open) then
				backtrack(__add(current, ")"), open, __add(close, 1))
			end
		end
	end
	backtrack("", 0, 0)
	return result
end

function test_example_1()
	if not (__eq(generateParenthesis(3), {"((()))", "(()())", "(())()", "()(())", "()()()"})) then error('expect failed') end
end

function test_example_2()
	if not (__eq(generateParenthesis(1), {"()"})) then error('expect failed') end
end

function test_two_pairs()
	if not (__eq(generateParenthesis(2), {"(())", "()()"})) then error('expect failed') end
end

test_example_1()
test_example_2()
test_two_pairs()
