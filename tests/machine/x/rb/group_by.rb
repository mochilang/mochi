require 'ostruct'

def _avg(v)
  list = nil
  if defined?(MGroup) && v.is_a?(MGroup)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.sum(0.0) / list.length
end
class MGroup
  include Enumerable
  attr_accessor :key, :Items
  def initialize(k)
    @key = k
    @Items = []
  end
  def length
    @Items.length
  end
  def items
    @Items
  end
  def each(&block)
    @Items.each(&block)
  end
end
def _group_by(src, keyfn)
grouped = src.group_by do |it|
  if it.is_a?(Array)
    keyfn.call(*it)
  else
    keyfn.call(it)
  end
end
grouped.map do |k, items|
g = MGroup.new(k)
items.each do |it|
  if it.is_a?(Array) && it.length == 1
    g.Items << it[0]
  else
    g.Items << it
  end
end
g
end
end

$people = [OpenStruct.new(name: "Alice", age: 30, city: "Paris"), OpenStruct.new(name: "Bob", age: 15, city: "Hanoi"), OpenStruct.new(name: "Charlie", age: 65, city: "Paris"), OpenStruct.new(name: "Diana", age: 45, city: "Hanoi"), OpenStruct.new(name: "Eve", age: 70, city: "Paris"), OpenStruct.new(name: "Frank", age: 22, city: "Hanoi")]
$stats = _group_by($people, ->(person){ person.city }).map { |g| OpenStruct.new(city: g.key, count: (g).length, avg_age: _avg(((g)).map { |p| p.age })) }
puts("--- People grouped by city ---")
$stats.each do |s|
	puts([s.city, ": count =", s.count, ", avg_age =", s.avg_age].join(" "))
end
