def find_itinerary(tickets)
  graph = Hash.new { |h, k| h[k] = [] }
  tickets.each { |a, b| graph[a] << b }
  graph.each_value { |v| v.sort!.reverse! }
  route = []
  visit = lambda do |a|
    visit.call(graph[a].pop) until graph[a].empty?
    route << a
  end
  visit.call('JFK')
  route.reverse
end
def fmt(route) '[' + route.map { |s| '"' + s + '"' }.join(',') + ']' end
data = STDIN.read.split
if data.length > 0
  idx = 0; t = data[idx].to_i; idx += 1; out = []
  t.times do
    m = data[idx].to_i; idx += 1; tickets = []
    m.times { tickets << [data[idx], data[idx + 1]]; idx += 2 }
    out << fmt(find_itinerary(tickets))
  end
  print out.join("\n\n")
end
