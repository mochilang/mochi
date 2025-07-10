import java.util.*;
public class Main {
	static List<Map<Object,Object>> nation = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(Map.ofEntries(Map.entry("n_nationkey", 1), Map.entry("n_name", "BRAZIL")))));
	static List<Map<Object,Object>> customer = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(Map.ofEntries(Map.entry("c_custkey", 1), Map.entry("c_name", "Alice"), Map.entry("c_acctbal", 100.000000), Map.entry("c_nationkey", 1), Map.entry("c_address", "123 St"), Map.entry("c_phone", "123-456"), Map.entry("c_comment", "Loyal")))));
	static List<Map<Object,Object>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(Map.ofEntries(Map.entry("o_orderkey", 1000), Map.entry("o_custkey", 1), Map.entry("o_orderdate", "1993-10-15"))), new LinkedHashMap<>(Map.ofEntries(Map.entry("o_orderkey", 2000), Map.entry("o_custkey", 1), Map.entry("o_orderdate", "1994-01-02")))));
	static List<Map<Object,Object>> lineitem = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(Map.ofEntries(Map.entry("l_orderkey", 1000), Map.entry("l_returnflag", "R"), Map.entry("l_extendedprice", 1000.000000), Map.entry("l_discount", 0.100000))), new LinkedHashMap<>(Map.ofEntries(Map.entry("l_orderkey", 2000), Map.entry("l_returnflag", "N"), Map.entry("l_extendedprice", 500.000000), Map.entry("l_discount", 0.000000)))));
	static String start_date = "1993-10-01";
	static String end_date = "1994-01-01";
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	Map<Object,List<Object>> _groups7 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(Objects.equals(((Map)o).get("o_custkey"), ((Map)c).get("c_custkey")))) continue;
			for (var l : lineitem) {
				if (!(Objects.equals(((Map)l).get("l_orderkey"), ((Map)o).get("o_orderkey")))) continue;
				for (var n : nation) {
					if (!(Objects.equals(((Map)n).get("n_nationkey"), ((Map)c).get("c_nationkey")))) continue;
					if (!(Boolean.TRUE.equals(Objects.equals(Boolean.TRUE.equals(String.valueOf(Boolean.TRUE.equals(String.valueOf(((Map)o).get("o_orderdate")).compareTo(String.valueOf(start_date)) >= 0) && Boolean.TRUE.equals(((Map)o).get("o_orderdate"))).compareTo(String.valueOf(end_date)) < 0) && Boolean.TRUE.equals(((Map)l).get("l_returnflag")), "R")))) continue;
					Map<String,Object> _row8 = new HashMap<>();
					_row8.put("c", c);
					_row8.put("o", o);
					_row8.put("l", l);
					_row8.put("n", n);
					Object _key9 = new LinkedHashMap<>(Map.ofEntries(Map.entry("c_custkey", ((Map)c).get("c_custkey")), Map.entry("c_name", ((Map)c).get("c_name")), Map.entry("c_acctbal", ((Map)c).get("c_acctbal")), Map.entry("c_address", ((Map)c).get("c_address")), Map.entry("c_phone", ((Map)c).get("c_phone")), Map.entry("c_comment", ((Map)c).get("c_comment")), Map.entry("n_name", ((Map)n).get("n_name"))));
					List<Object> _b10 = _groups7.get(_key9);
					if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
					_b10.add(_row8);
				}
			}
		}
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res6.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("c_custkey", ((Map)g_key).get("c_custkey")), Map.entry("c_name", ((Map)g_key).get("c_name")), Map.entry("revenue", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(((Number)((Map)((Map)x).get("l")).get("l_extendedprice")).doubleValue() * ((Number)(1 - ((Number)((Map)((Map)x).get("l")).get("l_discount")).doubleValue())).doubleValue());
	}
	return _res11;
}}).get())), Map.entry("c_acctbal", ((Map)g_key).get("c_acctbal")), Map.entry("n_name", ((Map)g_key).get("n_name")), Map.entry("c_address", ((Map)g_key).get("c_address")), Map.entry("c_phone", ((Map)g_key).get("c_phone")), Map.entry("c_comment", ((Map)g_key).get("c_comment")))));
	}
	return _res6;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
