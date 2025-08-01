# Generated by Mochi compiler v0.10.26 on 2025-07-15T06:46:59Z
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

$date_dim = [OpenStruct.new(d_date_sk: 1, d_date: 1)]
$store_sales = [OpenStruct.new(ss_sold_date_sk: 1, s_store_sk: 1, ss_ext_sales_price: 100.0, ss_net_profit: 10.0)]
$store_returns = [OpenStruct.new(sr_returned_date_sk: 1, s_store_sk: 1, sr_return_amt: 5.0, sr_net_loss: 1.0)]
$catalog_sales = [OpenStruct.new(cs_sold_date_sk: 1, cs_call_center_sk: 1, cs_ext_sales_price: 150.0, cs_net_profit: 15.0)]
$catalog_returns = [OpenStruct.new(cr_returned_date_sk: 1, cr_call_center_sk: 1, cr_return_amount: 7.0, cr_net_loss: 3.0)]
$web_sales = [OpenStruct.new(ws_sold_date_sk: 1, ws_web_page_sk: 1, ws_ext_sales_price: 200.0, ws_net_profit: 20.0)]
$web_returns = [OpenStruct.new(wr_returned_date_sk: 1, wr_web_page_sk: 1, wr_return_amt: 10.0, wr_net_loss: 2.0)]
$ss = (begin
	src = $store_sales
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(ss, d){ (d.d_date_sk == $ss.ss_sold_date_sk) } }
	], { 'select' => ->(ss, d){ [ss, d] } })
	_groups = _group_by(_rows, ->(ss, d){ $ss.s_store_sk })
	_items0 = _groups
	_res = []
	for g in _items0
		_res << OpenStruct.new(s_store_sk: g.key, sales: _sum(((g)).map { |x| x[0].ss_ext_sales_price }), profit: _sum(((g)).map { |x| x[0].ss_net_profit }))
	end
	_res
end)
$sr = (begin
	src = $store_returns
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(sr, d){ (d.d_date_sk == $sr.sr_returned_date_sk) } }
	], { 'select' => ->(sr, d){ [sr, d] } })
	_groups = _group_by(_rows, ->(sr, d){ $sr.s_store_sk })
	_items1 = _groups
	_res = []
	for g in _items1
		_res << OpenStruct.new(s_store_sk: g.key, returns: _sum(((g)).map { |x| x[0].sr_return_amt }), profit_loss: _sum(((g)).map { |x| x[0].sr_net_loss }))
	end
	_res
end)
$cs = (begin
	src = $catalog_sales
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(cs, d){ (d.d_date_sk == $cs.cs_sold_date_sk) } }
	], { 'select' => ->(cs, d){ [cs, d] } })
	_groups = _group_by(_rows, ->(cs, d){ $cs.cs_call_center_sk })
	_items2 = _groups
	_res = []
	for g in _items2
		_res << OpenStruct.new(cs_call_center_sk: g.key, sales: _sum(((g)).map { |x| x[0].cs_ext_sales_price }), profit: _sum(((g)).map { |x| x[0].cs_net_profit }))
	end
	_res
end)
$cr = (begin
	src = $catalog_returns
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(cr, d){ (d.d_date_sk == $cr.cr_returned_date_sk) } }
	], { 'select' => ->(cr, d){ [cr, d] } })
	_groups = _group_by(_rows, ->(cr, d){ $cr.cr_call_center_sk })
	_items3 = _groups
	_res = []
	for g in _items3
		_res << OpenStruct.new(cr_call_center_sk: g.key, returns: _sum(((g)).map { |x| x[0].cr_return_amount }), profit_loss: _sum(((g)).map { |x| x[0].cr_net_loss }))
	end
	_res
end)
$ws = (begin
	src = $web_sales
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(ws, d){ (d.d_date_sk == $ws.ws_sold_date_sk) } }
	], { 'select' => ->(ws, d){ [ws, d] } })
	_groups = _group_by(_rows, ->(ws, d){ $ws.ws_web_page_sk })
	_items4 = _groups
	_res = []
	for g in _items4
		_res << OpenStruct.new(wp_web_page_sk: g.key, sales: _sum(((g)).map { |x| x[0].ws_ext_sales_price }), profit: _sum(((g)).map { |x| x[0].ws_net_profit }))
	end
	_res
end)
$wr = (begin
	src = $web_returns
	_rows = _query(src, [
		{ 'items' => $date_dim, 'on' => ->(wr, d){ (d.d_date_sk == $wr.wr_returned_date_sk) } }
	], { 'select' => ->(wr, d){ [wr, d] } })
	_groups = _group_by(_rows, ->(wr, d){ $wr.wr_web_page_sk })
	_items5 = _groups
	_res = []
	for g in _items5
		_res << OpenStruct.new(wp_web_page_sk: g.key, returns: _sum(((g)).map { |x| x[0].wr_return_amt }), profit_loss: _sum(((g)).map { |x| x[0].wr_net_loss }))
	end
	_res
end)
$per_channel = (((begin
	src = $ss
	_rows = _query(src, [
		{ 'items' => $sr, 'on' => ->(s, r){ (s.s_store_sk == r.s_store_sk) }, 'left' => true }
	], { 'select' => ->(s, r){ OpenStruct.new(channel: "store channel", id: s.s_store_sk, sales: s.sales, returns: ((r == nil) ? 0.0 : r.returns), profit: (s.profit - (((r == nil) ? 0.0 : r.profit_loss)))) } })
	_rows
end) + (begin
	_res = []
	for c in $cs
		for r in $cr
			if (c.cs_call_center_sk == r.cr_call_center_sk)
				_res << OpenStruct.new(channel: "catalog channel", id: c.cs_call_center_sk, sales: c.sales, returns: r.returns, profit: (c.profit - r.profit_loss))
			end
		end
	end
	_res
end)) + (begin
	src = $ws
	_rows = _query(src, [
		{ 'items' => $wr, 'on' => ->(w, r){ (w.wp_web_page_sk == r.wp_web_page_sk) }, 'left' => true }
	], { 'select' => ->(w, r){ OpenStruct.new(channel: "web channel", id: w.wp_web_page_sk, sales: w.sales, returns: ((r == nil) ? 0.0 : r.returns), profit: (w.profit - (((r == nil) ? 0.0 : r.profit_loss)))) } })
	_rows
end))
$result = (begin
	src = $per_channel
	_rows = _query(src, [
	], { 'select' => ->(p){ [p] } })
	_groups = _group_by(_rows, ->(p){ OpenStruct.new(channel: p.channel, id: p.id) })
	_items6 = _groups
	_items6 = _items6.sort_by { |g| g.key.channel }
	_res = []
	for g in _items6
		_res << OpenStruct.new(channel: g.key.channel, id: g.key.id, sales: _sum(((g)).map { |x| x[0].sales }), returns: _sum(((g)).map { |x| x[0].returns }), profit: _sum(((g)).map { |x| x[0].profit }))
	end
	_res
end)
_json($result)
raise "expect failed" unless ($result == [OpenStruct.new(channel: "catalog channel", id: 1, sales: 150.0, returns: 7.0, profit: 12.0), OpenStruct.new(channel: "store channel", id: 1, sales: 100.0, returns: 5.0, profit: 9.0), OpenStruct.new(channel: "web channel", id: 1, sales: 200.0, returns: 10.0, profit: 18.0)])
