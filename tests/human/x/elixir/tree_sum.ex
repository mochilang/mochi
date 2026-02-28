defmodule Tree do
  defstruct [:left, :value, :right]
end

def sum_tree(tree) do
  case tree do
    :leaf -> 0
    %Tree{left: l, value: v, right: r} -> sum_tree(l) + v + sum_tree(r)
  end
end

t = %Tree{
  left: :leaf,
  value: 1,
  right: %Tree{left: :leaf, value: 2, right: :leaf}
}

IO.inspect(sum_tree(t))
