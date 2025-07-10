import java.util.*;
class COLN {
	Map<String,Object> c;
	Map<String,Object> o;
	Map<String,Object> l;
	Map<String,Object> n;
	COLN(Map<String,Object> c, Map<String,Object> o, Map<String,Object> l, Map<String,Object> n) {
		this.c = c;
		this.o = o;
		this.l = l;
		this.n = n;
	}
}
public class Main {
	static List<Map<String,Object>> nation = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("n_nationkey", 1), entry("n_name", "BRAZIL"))));
	static List<Map<String,Object>> customer = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("c_custkey", 1), entry("c_name", "Alice"), entry("c_acctbal", 100.000000), entry("c_nationkey", 1), entry("c_address", "123 St"), entry("c_phone", "123-456"), entry("c_comment", "Loyal"))));
	static List<Map<String,Object>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("o_orderkey", 1000), entry("o_custkey", 1), entry("o_orderdate", "1993-10-15")), mapOfEntries(entry("o_orderkey", 2000), entry("o_custkey", 1), entry("o_orderdate", "1994-01-02"))));
	static List<Map<String,Object>> lineitem = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("l_orderkey", 1000), entry("l_returnflag", "R"), entry("l_extendedprice", 1000.000000), entry("l_discount", 0.100000)), mapOfEntries(entry("l_orderkey", 2000), entry("l_returnflag", "N"), entry("l_extendedprice", 500.000000), entry("l_discount", 0.000000))));
	static String start_date = "1993-10-01";
	static String end_date = "1994-01-01";
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res6 = new ArrayList<>();
	Map<Map<String,Object>,List<COLN>> _groups7 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(Objects.equals(((Map)o).get("o_custkey"), ((Map)c).get("c_custkey")))) continue;
			for (var l : lineitem) {
				if (!(Objects.equals(((Map)l).get("l_orderkey"), ((Map)o).get("o_orderkey")))) continue;
				for (var n : nation) {
					if (!(Objects.equals(((Map)n).get("n_nationkey"), ((Map)c).get("c_nationkey")))) continue;
					if (!(Objects.equals(String.valueOf(String.valueOf(((Map)o).get("o_orderdate")).compareTo(String.valueOf(start_date)) >= 0 && ((Map)o).get("o_orderdate") != null).compareTo(String.valueOf(end_date)) < 0 && ((Map)l).get("l_returnflag") != null, "R"))) continue;
					COLN _row8 = new COLN(c, o, l, n);
					Map<String,Object> _key9 = mapOfEntries(entry("c_custkey", ((Map)c).get("c_custkey")), entry("c_name", ((Map)c).get("c_name")), entry("c_acctbal", ((Map)c).get("c_acctbal")), entry("c_address", ((Map)c).get("c_address")), entry("c_phone", ((Map)c).get("c_phone")), entry("c_comment", ((Map)c).get("c_comment")), entry("n_name", ((Map)n).get("n_name")));
					List<COLN> _b10 = _groups7.get(_key9);
					if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
					_b10.add(_row8);
				}
			}
		}
	}
	for (var __e : _groups7.entrySet()) {
		Map<String,Object> g_key = __e.getKey();
		List<COLN> g = __e.getValue();
		_res6.add(mapOfEntries(entry("c_custkey", ((Map)g_key).get("c_custkey")), entry("c_name", ((Map)g_key).get("c_name")), entry("revenue", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(x.l.l_extendedprice * (1 - x.l.l_discount));
	}
	return _res11;
}}).get())), entry("c_acctbal", ((Map)g_key).get("c_acctbal")), entry("n_name", ((Map)g_key).get("n_name")), entry("c_address", ((Map)g_key).get("c_address")), entry("c_phone", ((Map)g_key).get("c_phone")), entry("c_comment", ((Map)g_key).get("c_comment"))));
	}
	return _res6;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
