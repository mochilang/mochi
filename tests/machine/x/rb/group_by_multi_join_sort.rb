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
def _query(src, joins, opts)
  where_fn = opts['where']
  items = []
  if joins.empty?
    src.each do |v|
      row = [v]
      next if where_fn && !where_fn.call(*row)
      items << row
    end
  else
    items = src.map { |v| [v] }
    joins.each_with_index do |j, idx|
      joined = []
      jitems = j['items']
      on = j['on']
      left = j['left']
      right = j['right']
      last = idx == joins.length - 1
      if right && left
        matched = Array.new(jitems.length, false)
        items.each do |l|
          m = false
          jitems.each_with_index do |r, ri|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            matched[ri] = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          row = l + [nil]
          if left && !m
            if last && where_fn && !where_fn.call(*row)
              # skip
            else
              joined << row
            end
          end
        end
        jitems.each_with_index do |r, ri|
          next if matched[ri]
          _undef = Array.new(items[0]&.length || 0, nil)
          row = _undef + [r]
          if last && where_fn && !where_fn.call(*row)
            next
          end
          joined << row
        end
      elsif right
        jitems.each do |r|
          m = false
          items.each do |l|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          unless m
            _undef = Array.new(items[0]&.length || 0, nil)
            row = _undef + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
        end
      else
        items.each do |l|
          m = false
          jitems.each do |r|
            keep = true
            keep = on.call(*l, r) if on
            next unless keep
            m = true
            row = l + [r]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
          if left && !m
            row = l + [nil]
            if last && where_fn && !where_fn.call(*row)
              next
            end
            joined << row
          end
        end
      end
      items = joined
    end
  end
  if opts['sortKey']
    items = items.map { |it| [it, opts['sortKey'].call(*it)] }
    items.sort_by! { |p| p[1] }
    items.map!(&:first)
  end
  if opts.key?('skip')
    n = opts['skip']
    items = n < items.length ? items[n..-1] : []
  end
  if opts.key?('take')
    n = opts['take']
    items = n < items.length ? items[0...n] : items
  end
  res = []
  items.each { |r| res << opts['select'].call(*r) }
  res
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

nation = [OpenStruct.new(n_nationkey: 1, n_name: "BRAZIL")]
customer = [OpenStruct.new(c_custkey: 1, c_name: "Alice", c_acctbal: 100.0, c_nationkey: 1, c_address: "123 St", c_phone: "123-456", c_comment: "Loyal")]
orders = [OpenStruct.new(o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15"), OpenStruct.new(o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02")]
lineitem = [OpenStruct.new(l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1), OpenStruct.new(l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0)]
start_date = "1993-10-01"
end_date = "1994-01-01"
result = (begin
	src = customer
	_rows = _query(src, [
		{ 'items' => orders, 'on' => ->(c, o){ (o.o_custkey == c.c_custkey) } },
		{ 'items' => lineitem, 'on' => ->(c, o, l){ (l.l_orderkey == o.o_orderkey) } },
		{ 'items' => nation, 'on' => ->(c, o, l, n){ (n.n_nationkey == c.c_nationkey) } }
	], { 'select' => ->(c, o, l, n){ [c, o, l, n] }, 'where' => ->(c, o, l, n){ (((o.o_orderdate >= start_date) && (o.o_orderdate < end_date)) && (l.l_returnflag == "R")) }, 'sortKey' => ->(c, o, l, n){ (-_sum(((g)).map { |x| (x.l.l_extendedprice * ((1 - x.l.l_discount))) })) } })
	_groups = _group_by(_rows, ->(c, o, l, n){ OpenStruct.new(c_custkey: c.c_custkey, c_name: c.c_name, c_acctbal: c.c_acctbal, c_address: c.c_address, c_phone: c.c_phone, c_comment: c.c_comment, n_name: n.n_name) })
	_res = []
	for g in _groups
		_res << OpenStruct.new(c_custkey: g.key.c_custkey, c_name: g.key.c_name, revenue: _sum(((g)).map { |x| (x.l.l_extendedprice * ((1 - x.l.l_discount))) }), c_acctbal: g.key.c_acctbal, n_name: g.key.n_name, c_address: g.key.c_address, c_phone: g.key.c_phone, c_comment: g.key.c_comment)
	end
	_res
end)
puts([result].join(" "))
