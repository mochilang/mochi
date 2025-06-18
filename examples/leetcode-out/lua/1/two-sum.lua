function __print(...)
	local args = {...}
	for i, a in ipairs(args) do
		if i > 1 then io.write(' ') end
		io.write(tostring(a))
	end
	io.write('\n')
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

function twoSum(nums, target)
	local n = #nums
	for i = 0, (n)-1 do
		for j = __add(i, 1), (n)-1 do
			if __eq(__add(__index(nums, i), __index(nums, j)), target) then
				return {i, j}
			end
			::__continue1::
		end
		::__continue0::
	end
	return {-1, -1}
end

local result = twoSum({2, 7, 11, 15}, 9)
__print(__index(result, 0))
__print(__index(result, 1))
