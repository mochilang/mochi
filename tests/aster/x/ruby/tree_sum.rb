# Generated by Mochi transpiler v0.10.33 on 2025-07-21 16:57 +0700
Leaf = nil
Node = Struct.new(:left, :value, :right, keyword_init: true)
def sum_tree(t)
  return (t == Leaf ? 0 : (t != nil ? sum_tree(t["left"]) + t["value"] + sum_tree(t["right"]) : nil))
end
t = Node.new(left: Leaf, value: 1, right: Node.new(left: Leaf, value: 2, right: Leaf))
puts(sum_tree(t))
