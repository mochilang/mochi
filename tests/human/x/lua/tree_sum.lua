local function sum_tree(t)
  if t.kind == "Leaf" then
    return 0
  else
    return sum_tree(t.left) + t.value + sum_tree(t.right)
  end
end

local t = {
  kind = "Node",
  left = {kind = "Leaf"},
  value = 1,
  right = {
    kind = "Node",
    left = {kind = "Leaf"},
    value = 2,
    right = {kind = "Leaf"}
  }
}

print(sum_tree(t))
