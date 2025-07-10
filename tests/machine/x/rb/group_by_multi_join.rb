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
def _sum(v)
  list = nil
  if defined?(MGroup) && v.is_a?(MGroup)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  s = 0.0
  list.each { |n| s += n.to_f }
  s
end

nations = [OpenStruct.new(id: 1, name: "A"), OpenStruct.new(id: 2, name: "B")]
suppliers = [OpenStruct.new(id: 1, nation: 1), OpenStruct.new(id: 2, nation: 2)]
partsupp = [OpenStruct.new(part: 100, supplier: 1, cost: 10.0, qty: 2), OpenStruct.new(part: 100, supplier: 2, cost: 20.0, qty: 1), OpenStruct.new(part: 200, supplier: 1, cost: 5.0, qty: 3)]
filtered = (begin
	_res = []
	for ps in partsupp
		for s in suppliers
			if (s.id == ps.supplier)
				for n in nations
					if (n.id == s.nation)
						if (n.name == "A")
							_res << OpenStruct.new(part: ps.part, value: (ps.cost * ps.qty))
						end
					end
				end
			end
		end
	end
	_res
end)
grouped = _group_by(filtered, ->(x){ x.part }).map { |g| OpenStruct.new(part: g.key, total: _sum(((g)).map { |r| r.value })) }
puts(grouped)
