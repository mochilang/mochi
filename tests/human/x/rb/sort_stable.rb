items = [
  {n: 1, v: 'a'},
  {n: 1, v: 'b'},
  {n: 2, v: 'c'}
]

result = items.each_with_index.sort_by { |x,i| [x[:n], i] }.map { |x,i| x[:v] }

p result
