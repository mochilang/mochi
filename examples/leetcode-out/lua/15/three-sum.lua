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

function threeSum(nums)
	local sorted = (function()
	local items = {}
	for _, x in ipairs(nums) do
		table.insert(items, x)
	end
	table.sort(items, function(a, b)
		local x = a
		local _ka = x
		x = b
		local _kb = x
		return (_ka) < (_kb)
	end)
	local _res = {}
	for _, x in ipairs(items) do
		table.insert(_res, x)
	end
	return _res
end)()
	local n = #sorted
	local res = {}
	local i = 0
	while (i < n) do
		if ((i > 0) and __eq(__index(sorted, i), __index(sorted, (i - 1)))) then
			i = __add(i, 1)
			goto __continue0
		end
		local left = __add(i, 1)
		local right = (n - 1)
		while (left < right) do
			local sum = __add(__add(__index(sorted, i), __index(sorted, left)), __index(sorted, right))
			if __eq(sum, 0) then
				res = __add(res, {{__index(sorted, i), __index(sorted, left), __index(sorted, right)}})
				left = __add(left, 1)
				while ((left < right) and __eq(__index(sorted, left), __index(sorted, (left - 1)))) do
					left = __add(left, 1)
					::__continue2::
				end
				right = (right - 1)
				while ((left < right) and __eq(__index(sorted, right), __index(sorted, __add(right, 1)))) do
					right = (right - 1)
					::__continue3::
				end
			elseif (sum < 0) then
				left = __add(left, 1)
			else
				right = (right - 1)
			end
			::__continue1::
		end
		i = __add(i, 1)
		::__continue0::
	end
	return res
end

function test_example_1()
	if not (__eq(threeSum({-1, 0, 1, 2, -1, -4}), {{-1, -1, 2}, {-1, 0, 1}})) then error('expect failed') end
end

function test_example_2()
	if not (__eq(threeSum({0, 1, 1}), {})) then error('expect failed') end
end

function test_example_3()
	if not (__eq(threeSum({0, 0, 0}), {{0, 0, 0}})) then error('expect failed') end
end

test_example_1()
test_example_2()
test_example_3()
