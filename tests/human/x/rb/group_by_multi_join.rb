nations = [
  {id: 1, name: 'A'},
  {id: 2, name: 'B'}
]
suppliers = [
  {id: 1, nation: 1},
  {id: 2, nation: 2}
]
partsupp = [
  {part: 100, supplier: 1, cost: 10.0, qty: 2},
  {part: 100, supplier: 2, cost: 20.0, qty: 1},
  {part: 200, supplier: 1, cost: 5.0, qty: 3}
]

filtered = []
partsupp.each do |ps|
  s = suppliers.find { |sup| sup[:id] == ps[:supplier] }
  next unless s
  n = nations.find { |na| na[:id] == s[:nation] }
  next unless n && n[:name] == 'A'
  filtered << { part: ps[:part], value: ps[:cost] * ps[:qty] }
end

grouped = filtered.group_by { |x| x[:part] }.map do |part, g|
  { part: part, total: g.sum { |r| r[:value] } }
end

p grouped
