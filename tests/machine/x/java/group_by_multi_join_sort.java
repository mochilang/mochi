import java.util.*;
public class Main {
	static List<Map<String,Object>> nation = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("n_nationkey", 1, "n_name", "BRAZIL")));
	static List<Map<String,Object>> customer = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("c_custkey", 1, "c_name", "Alice", "c_acctbal", 100.000000, "c_nationkey", 1, "c_address", "123 St", "c_phone", "123-456", "c_comment", "Loyal")));
	static List<Map<String,Object>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("o_orderkey", 1000, "o_custkey", 1, "o_orderdate", "1993-10-15"), Main.<String,Object>mapOf("o_orderkey", 2000, "o_custkey", 1, "o_orderdate", "1994-01-02")));
	static List<Map<String,Object>> lineitem = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("l_orderkey", 1000, "l_returnflag", "R", "l_extendedprice", 1000.000000, "l_discount", 0.100000), Main.<String,Object>mapOf("l_orderkey", 2000, "l_returnflag", "N", "l_extendedprice", 500.000000, "l_discount", 0.000000)));
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
					if (!(Objects.equals(String.valueOf(String.valueOf(((Map)o).get("o_orderdate")).compareTo(String.valueOf(start_date)) >= 0 && ((Map)o).get("o_orderdate") != null).compareTo(String.valueOf(end_date)) < 0 && ((Map)l).get("l_returnflag") != null, "R"))) continue;
					Map<String,Object> _row8 = new HashMap<>();
					_row8.put("c", c);
					_row8.put("o", o);
					_row8.put("l", l);
					_row8.put("n", n);
					Object _key9 = Main.<String,Object>mapOf("c_custkey", ((Map)c).get("c_custkey"), "c_name", ((Map)c).get("c_name"), "c_acctbal", ((Map)c).get("c_acctbal"), "c_address", ((Map)c).get("c_address"), "c_phone", ((Map)c).get("c_phone"), "c_comment", ((Map)c).get("c_comment"), "n_name", ((Map)n).get("n_name"));
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
		_res6.add(Main.<String,Object>mapOf("c_custkey", ((Map)g_key).get("c_custkey"), "c_name", ((Map)g_key).get("c_name"), "revenue", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(((Number)((Map)((Map)x).get("l")).get("l_extendedprice")).doubleValue() * ((Number)(1 - ((Number)((Map)((Map)x).get("l")).get("l_discount")).doubleValue())).doubleValue());
	}
	return _res11;
}}).get()), "c_acctbal", ((Map)g_key).get("c_acctbal"), "n_name", ((Map)g_key).get("n_name"), "c_address", ((Map)g_key).get("c_address"), "c_phone", ((Map)g_key).get("c_phone"), "c_comment", ((Map)g_key).get("c_comment")));
	}
	return _res6;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
