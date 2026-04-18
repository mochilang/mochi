function removeDuplicates(nums)
    if #nums == 0 then return 0 end
    local k = 1
    for i = 2, #nums do
        if nums[i] ~= nums[k] then
            k = k + 1
            nums[k] = nums[i]
        end
    end
    return k
end

local input = io.read("*all")
local tokens = {}
for token in input:gmatch("%S+") do
    table.insert(tokens, tonumber(token))
end

if #tokens == 0 then return end
local idx = 1
local t = tokens[idx]
idx = idx + 1
for tc = 1, t do
    local n = tokens[idx]
    idx = idx + 1
    local nums = {}
    for i = 1, n do
        table.insert(nums, tokens[idx])
        idx = idx + 1
    end
    local k = removeDuplicates(nums)
    local res = {}
    for i = 1, k do
        table.insert(res, tostring(nums[i]))
    end
    print(table.concat(res, " "))
end
