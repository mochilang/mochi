import java.util.*;
public class Main {
	static List<Map<Object,Object>> nation = java.util.Arrays.asList(map("n_nationkey", 1, "n_name", "BRAZIL"));
	static List<Map<Object,Object>> customer = java.util.Arrays.asList(map("c_custkey", 1, "c_name", "Alice", "c_acctbal", 100.000000, "c_nationkey", 1, "c_address", "123 St", "c_phone", "123-456", "c_comment", "Loyal"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("o_orderkey", 1000, "o_custkey", 1, "o_orderdate", "1993-10-15"), map("o_orderkey", 2000, "o_custkey", 1, "o_orderdate", "1994-01-02"));
	static List<Map<Object,Object>> lineitem = java.util.Arrays.asList(map("l_orderkey", 1000, "l_returnflag", "R", "l_extendedprice", 1000.000000, "l_discount", 0.100000), map("l_orderkey", 2000, "l_returnflag", "N", "l_extendedprice", 500.000000, "l_discount", 0.000000));
	static String start_date = "1993-10-01";
	static String end_date = "1994-01-01";
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(((Map)o).get("o_custkey") == ((Map)c).get("c_custkey"))) continue;
			for (var l : lineitem) {
				if (!(((Map)l).get("l_orderkey") == ((Map)o).get("o_orderkey"))) continue;
				for (var n : nation) {
					if (!(((Map)n).get("n_nationkey") == ((Map)c).get("c_nationkey"))) continue;
					if (!(Boolean.TRUE.equals(Boolean.TRUE.equals(((Number)Boolean.TRUE.equals(((Number)((Map)o).get("o_orderdate")).doubleValue() >= start_date) && Boolean.TRUE.equals(((Map)o).get("o_orderdate"))).doubleValue() < end_date) && Boolean.TRUE.equals(((Map)l).get("l_returnflag")) == "R"))) continue;
					Map<String,Object> _row2 = new HashMap<>();
					_row2.put("c", c);
					_row2.put("o", o);
					_row2.put("l", l);
					_row2.put("n", n);
					Object _key3 = map("c_custkey", ((Map)c).get("c_custkey"), "c_name", ((Map)c).get("c_name"), "c_acctbal", ((Map)c).get("c_acctbal"), "c_address", ((Map)c).get("c_address"), "c_phone", ((Map)c).get("c_phone"), "c_comment", ((Map)c).get("c_comment"), "n_name", ((Map)n).get("n_name"));
					List<Object> _b4 = _groups1.get(_key3);
					if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
					_b4.add(_row2);
				}
			}
		}
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(map("c_custkey", g.key.c_custkey, "c_name", g.key.c_name, "revenue", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	for (var x : g) {
		_res5.add(((Number)((Map)((Map)x).get("l")).get("l_extendedprice")).doubleValue() * ((Number)(1 - ((Number)((Map)((Map)x).get("l")).get("l_discount")).doubleValue())).doubleValue());
	}
	return _res5;
}}).get()), "c_acctbal", g.key.c_acctbal, "n_name", g.key.n_name, "c_address", g.key.c_address, "c_phone", g.key.c_phone, "c_comment", g.key.c_comment));
	}
	return _res0;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
