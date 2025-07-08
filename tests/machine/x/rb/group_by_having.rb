require 'ostruct'

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
def _json(v)
  require 'json'
  obj = v
  if v.is_a?(Array)
    obj = v.map { |it| it.respond_to?(:to_h) ? it.to_h : it }
  elsif v.respond_to?(:to_h)
    obj = v.to_h
  end
  puts(JSON.generate(obj))
end

people = [OpenStruct.new(name: "Alice", city: "Paris"), OpenStruct.new(name: "Bob", city: "Hanoi"), OpenStruct.new(name: "Charlie", city: "Paris"), OpenStruct.new(name: "Diana", city: "Hanoi"), OpenStruct.new(name: "Eve", city: "Paris"), OpenStruct.new(name: "Frank", city: "Hanoi"), OpenStruct.new(name: "George", city: "Paris")]
big = _group_by(people, ->(p){ p.city }).map { |g| OpenStruct.new(city: g.key, num: (g).length) }
_json(big)
