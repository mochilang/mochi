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

function findMedianSortedArrays(nums1, nums2)
	local merged = {}
	local i = 0
	local j = 0
	while ((i < #nums1) or (j < #nums2)) do
		if (j >= #nums2) then
			merged = __add(merged, {__index(nums1, i)})
			i = __add(i, 1)
		elseif (i >= #nums1) then
			merged = __add(merged, {__index(nums2, j)})
			j = __add(j, 1)
		elseif (__index(nums1, i) <= __index(nums2, j)) then
			merged = __add(merged, {__index(nums1, i)})
			i = __add(i, 1)
		else
			merged = __add(merged, {__index(nums2, j)})
			j = __add(j, 1)
		end
		::__continue0::
	end
	local total = #merged
	if __eq((total % 2), 1) then
		return __index(merged, __div(total, 2))
	end
	local mid1 = __index(merged, (__div(total, 2) - 1))
	local mid2 = __index(merged, __div(total, 2))
	return __div((__add(mid1, mid2)), 2.0)
end

function test_example_1()
	if not (__eq(findMedianSortedArrays({1, 3}, {2}), 2.0)) then error('expect failed') end
end

function test_example_2()
	if not (__eq(findMedianSortedArrays({1, 2}, {3, 4}), 2.5)) then error('expect failed') end
end

function test_empty_first()
	if not (__eq(findMedianSortedArrays({}, {1}), 1.0)) then error('expect failed') end
end

function test_empty_second()
	if not (__eq(findMedianSortedArrays({2}, {}), 2.0)) then error('expect failed') end
end

test_example_1()
test_example_2()
test_empty_first()
test_empty_second()
