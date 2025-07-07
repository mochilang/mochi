data = [
  {tag: 'a', val: 1},
  {tag: 'a', val: 2},
  {tag: 'b', val: 3}
]

groups = data.group_by { |d| d[:tag] }
results = []
groups.each do |tag, items|
  total = 0
  items.each { |x| total += x[:val] }
  results << {tag: tag, total: total}
end
results.sort_by! { |r| r[:tag] }

p results
