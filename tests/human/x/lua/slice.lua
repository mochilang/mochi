local function slice(tbl, start_idx, end_idx)
  local res = {}
  for i = start_idx, end_idx - 1 do
    res[#res + 1] = tbl[i + 1]
  end
  return res
end

local function slice_str(s, start_idx, end_idx)
  return string.sub(s, start_idx + 1, end_idx)
end

local t = {1,2,3}
print(table.concat(slice(t, 1, 3), " "))
print(table.concat(slice(t, 0, 2), " "))
print(slice_str("hello", 1, 4))
