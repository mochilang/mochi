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
function sum_tree(t)
  return (function()
  local _t0 = t
  if _t0.__name == "Leaf" then return 0 end
  if _t0.__name == "Node" then return (function(left, value, right) return __add(__add(sum_tree(left), value), sum_tree(right)) end)(_t0.left, _t0.value, _t0.right) end
  return nil
end)()
end

t = {__name="Node", left={__name="Leaf"}, value=1, right={__name="Node", left={__name="Leaf"}, value=2, right={__name="Leaf"}}}
print(sum_tree(t))
