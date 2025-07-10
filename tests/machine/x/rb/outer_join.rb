require 'ostruct'

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

$customers = [OpenStruct.new(id: 1, name: "Alice"), OpenStruct.new(id: 2, name: "Bob"), OpenStruct.new(id: 3, name: "Charlie"), OpenStruct.new(id: 4, name: "Diana")]
$orders = [OpenStruct.new(id: 100, customerId: 1, total: 250), OpenStruct.new(id: 101, customerId: 2, total: 125), OpenStruct.new(id: 102, customerId: 1, total: 300), OpenStruct.new(id: 103, customerId: 5, total: 80)]
$result = (begin
	src = $orders
	_rows = _query(src, [
		{ 'items' => $customers, 'on' => ->(o, c){ (o.customerId == c.id) }, 'left' => true, 'right' => true }
	], { 'select' => ->(o, c){ OpenStruct.new(order: o, customer: c) } })
	_rows
end)
puts("--- Outer Join using syntax ---")
$result.each do |row|
  if row.order
    if row.customer
      puts(["Order", row.order.id, "by", row.customer.name, "- $", row.order.total].join(" "))
    else
      puts(["Order", row.order.id, "by", "Unknown", "- $", row.order.total].join(" "))
    end
  else
    puts(["Customer", row.customer.name, "has no orders"].join(" "))
  end
end
