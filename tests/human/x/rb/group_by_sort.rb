items = [
  {cat: 'a', val: 3},
  {cat: 'a', val: 1},
  {cat: 'b', val: 5},
  {cat: 'b', val: 2}
]

grouped = items.group_by { |i| i[:cat] }.map do |cat, g|
  { cat: cat, total: g.sum { |x| x[:val] } }
end

grouped.sort_by! { |h| -h[:total] }

p grouped
