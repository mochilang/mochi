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

function maxArea(height)
	local left = 0
	local right = (#height - 1)
	local maxArea = 0
	while (left < right) do
		local width = (right - left)
		local h = 0
		if (__index(height, left) < __index(height, right)) then
			h = __index(height, left)
		else
			h = __index(height, right)
		end
		local area = (h * width)
		if (area > maxArea) then
			maxArea = area
		end
		if (__index(height, left) < __index(height, right)) then
			left = __add(left, 1)
		else
			right = (right - 1)
		end
		::__continue0::
	end
	return maxArea
end

function test_example_1()
	if not (__eq(maxArea({1, 8, 6, 2, 5, 4, 8, 3, 7}), 49)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(maxArea({1, 1}), 1)) then error('expect failed') end
end

function test_decreasing_heights()
	if not (__eq(maxArea({4, 3, 2, 1, 4}), 16)) then error('expect failed') end
end

function test_short_array()
	if not (__eq(maxArea({1, 2, 1}), 2)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_decreasing_heights()
test_short_array()
