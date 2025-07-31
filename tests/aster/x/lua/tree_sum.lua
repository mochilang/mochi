function sum_tree(t);
  return (function(_m)
    if _m['__name'] == "Leaf" then
      return 0;
    elseif _m['__name'] == "Node" then
      local left = _m["left"];
      local value = _m["value"];
      local right = _m["right"];
      return ((sum_tree(left) + value) + sum_tree(right));
    end;
  end)(t);
end;
t = {__name = "Node", left = {__name = "Leaf"}, value = 1, right = {__name = "Node", left = {__name = "Leaf"}, value = 2, right = {__name = "Leaf"}}};
print(sum_tree(t));
