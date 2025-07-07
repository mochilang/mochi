data = [
  {a: 1, b: 2},
  {a: 1, b: 1},
  {a: 0, b: 5}
]

sorted = data.sort_by { |x| [x[:a], x[:b]] }

p sorted
