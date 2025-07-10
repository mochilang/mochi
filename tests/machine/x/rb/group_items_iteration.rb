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

$data = [OpenStruct.new(tag: "a", val: 1), OpenStruct.new(tag: "a", val: 2), OpenStruct.new(tag: "b", val: 3)]
$groups = _group_by($data, ->(d){ d.tag }).map { |g| g }
$tmp = []
$groups.each do |g|
  total = 0
  g.Items.each do |x|
    total = (total + x.val)
  end
  $tmp = ($tmp + [OpenStruct.new(tag: g.key, total: total)])
end
$result = ((($tmp)).sort_by { |r| r.tag }).map { |r| r }
puts($result)
