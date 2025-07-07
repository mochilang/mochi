class Leaf; end
class Node
  attr_accessor :left, :value, :right
  def initialize(left, value, right)
    @left = left
    @value = value
    @right = right
  end
end

def sum_tree(t)
  case t
  when Leaf
    0
  when Node
    sum_tree(t.left) + t.value + sum_tree(t.right)
  else
    0
  end
end

t = Node.new(Leaf.new, 1, Node.new(Leaf.new, 2, Leaf.new))
puts sum_tree(t)
