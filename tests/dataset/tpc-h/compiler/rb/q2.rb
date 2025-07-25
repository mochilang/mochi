# Generated by Mochi compiler v0.10.27 on 2025-07-17T06:40:56Z
require 'ostruct'

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
def _min(v)
  list = nil
  if v.respond_to?(:Items)
    list = v.Items
  elsif v.is_a?(Array)
    list = v
  elsif v.respond_to?(:to_a)
    list = v.to_a
  end
  return 0 if !list || list.empty?
  list.min
end

$region = [OpenStruct.new(r_regionkey: 1, r_name: "EUROPE"), OpenStruct.new(r_regionkey: 2, r_name: "ASIA")]
$nation = [OpenStruct.new(n_nationkey: 10, n_regionkey: 1, n_name: "FRANCE"), OpenStruct.new(n_nationkey: 20, n_regionkey: 2, n_name: "CHINA")]
$supplier = [OpenStruct.new(s_suppkey: 100, s_name: "BestSupplier", s_address: "123 Rue", s_nationkey: 10, s_phone: "123", s_acctbal: 1000.0, s_comment: "Fast and reliable"), OpenStruct.new(s_suppkey: 200, s_name: "AltSupplier", s_address: "456 Way", s_nationkey: 20, s_phone: "456", s_acctbal: 500.0, s_comment: "Slow")]
$part = [OpenStruct.new(p_partkey: 1000, p_type: "LARGE BRASS", p_size: 15, p_mfgr: "M1"), OpenStruct.new(p_partkey: 2000, p_type: "SMALL COPPER", p_size: 15, p_mfgr: "M2")]
$partsupp = [OpenStruct.new(ps_partkey: 1000, ps_suppkey: 100, ps_supplycost: 10.0), OpenStruct.new(ps_partkey: 1000, ps_suppkey: 200, ps_supplycost: 15.0)]
$europe_nations = (begin
	_res = []
	for r in $region
		for n in $nation
			if (n.n_regionkey == r.r_regionkey)
				if (r.r_name == "EUROPE")
					_res << n
				end
			end
		end
	end
	_res
end)
$europe_suppliers = (begin
	_res = []
	for s in $supplier
		for n in $europe_nations
			if (s.s_nationkey == n.n_nationkey)
				_res << OpenStruct.new(s: s, n: n)
			end
		end
	end
	_res
end)
$target_parts = ((($part)).select { |p| ((p.p_size == 15) && (p.p_type == "LARGE BRASS")) }).map { |p| p }
$target_partsupp = (begin
	_res = []
	for ps in $partsupp
		for p in $target_parts
			if (ps.ps_partkey == p.p_partkey)
				for s in $europe_suppliers
					if (ps.ps_suppkey == s.s.s_suppkey)
						_res << OpenStruct.new(s_acctbal: s.s.s_acctbal, s_name: s.s.s_name, n_name: s.n.n_name, p_partkey: p.p_partkey, p_mfgr: p.p_mfgr, s_address: s.s.s_address, s_phone: s.s.s_phone, s_comment: s.s.s_comment, ps_supplycost: ps.ps_supplycost)
					end
				end
			end
		end
	end
	_res
end)
$costs = (($target_partsupp)).map { |x| x.ps_supplycost }
$min_cost = _min($costs)
$result = (((($target_partsupp)).select { |x| (x.ps_supplycost == $min_cost) }).sort_by { |x| (-x.s_acctbal) }).map { |x| x }
_json($result)
raise "expect failed" unless ($result == [OpenStruct.new(s_acctbal: 1000.0, s_name: "BestSupplier", n_name: "FRANCE", p_partkey: 1000, p_mfgr: "M1", s_address: "123 Rue", s_phone: "123", s_comment: "Fast and reliable", ps_supplycost: 10.0)])
