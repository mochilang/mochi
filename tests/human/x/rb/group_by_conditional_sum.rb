items = [
  {cat: 'a', val: 10, flag: true},
  {cat: 'a', val: 5, flag: false},
  {cat: 'b', val: 20, flag: true}
]

groups = items.group_by { |i| i[:cat] }
result = groups.keys.sort.map do |k|
  g = groups[k]
  total = g.sum { |x| x[:val] }
  flagged = g.sum { |x| x[:flag] ? x[:val] : 0 }
  { cat: k, share: flagged.to_f / total }
end

p result
