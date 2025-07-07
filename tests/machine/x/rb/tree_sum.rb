module Tree; end
class Leaf
	include Tree
end
Node = Struct.new(:left, :value, :right, keyword_init: true) do
	include Tree
end

def sum_tree(t)
	return (begin
	_t0 = t
	case
	when _t0.is_a?(Leaf)
		0
	when _t0.is_a?(Node)
		(->(left, value, right){ ((sum_tree(left) + value) + sum_tree(right)) }).call(_t0.left, _t0.value, _t0.right)
	else
		nil
	end
end)
end

t = Node.new(left: method(:Leaf), value: 1, right: Node.new(left: method(:Leaf), value: 2, right: method(:Leaf)))
puts([sum_tree(t)].join(" "))
