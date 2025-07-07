def outer(x)
  inner = ->(y) { x + y }
  inner.call(5)
end
puts outer(3)
