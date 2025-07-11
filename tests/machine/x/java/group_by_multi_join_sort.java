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
	int size() { return 4; }
}
public class GroupByMultiJoinSort {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> nation = new ArrayList<>(Arrays.asList(mapOfEntries(entry("n_nationkey", 1), entry("n_name", "BRAZIL"))));
	List<Map<String,Object>> customer = new ArrayList<>(Arrays.asList(mapOfEntries(entry("c_custkey", 1), entry("c_name", "Alice"), entry("c_acctbal", 100.000000), entry("c_nationkey", 1), entry("c_address", "123 St"), entry("c_phone", "123-456"), entry("c_comment", "Loyal"))));
	List<Map<String,Object>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("o_orderkey", 1000), entry("o_custkey", 1), entry("o_orderdate", "1993-10-15")), mapOfEntries(entry("o_orderkey", 2000), entry("o_custkey", 1), entry("o_orderdate", "1994-01-02"))));
	List<Map<String,Object>> lineitem = new ArrayList<>(Arrays.asList(mapOfEntries(entry("l_orderkey", 1000), entry("l_returnflag", "R"), entry("l_extendedprice", 1000.000000), entry("l_discount", 0.100000)), mapOfEntries(entry("l_orderkey", 2000), entry("l_returnflag", "N"), entry("l_extendedprice", 500.000000), entry("l_discount", 0.000000))));
	String start_date = "1993-10-01";
	String end_date = "1994-01-01";
	List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<Map<String,Object>,List<COLN>> _groups1 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(Objects.equals(((Map)o).get("o_custkey"), ((Map)c).get("c_custkey")))) continue;
			for (var l : lineitem) {
				if (!(Objects.equals(((Map)l).get("l_orderkey"), ((Map)o).get("o_orderkey")))) continue;
				for (var n : nation) {
					if (!(Objects.equals(((Map)n).get("n_nationkey"), ((Map)c).get("c_nationkey")))) continue;
					if (!(Objects.equals(String.valueOf(String.valueOf(((Map)o).get("o_orderdate")).compareTo(String.valueOf(start_date)) >= 0 && ((Map)o).get("o_orderdate") != null).compareTo(String.valueOf(end_date)) < 0 && ((Map)l).get("l_returnflag") != null, "R"))) continue;
					COLN _row2 = new COLN(c, o, l, n);
					Map<String,Object> _key3 = mapOfEntries(entry("c_custkey", ((Map)c).get("c_custkey")), entry("c_name", ((Map)c).get("c_name")), entry("c_acctbal", ((Map)c).get("c_acctbal")), entry("c_address", ((Map)c).get("c_address")), entry("c_phone", ((Map)c).get("c_phone")), entry("c_comment", ((Map)c).get("c_comment")), entry("n_name", ((Map)n).get("n_name")));
					List<COLN> _b4 = _groups1.get(_key3);
					if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
					_b4.add(_row2);
				}
			}
		}
	}
	for (Map.Entry<Map<String,Object>,List<COLN>> __e : _groups1.entrySet()) {
		Map<String,Object> g_key = __e.getKey();
		List<COLN> g = __e.getValue();
		_res0.add(mapOfEntries(entry("c_custkey", ((Map)g_key).get("c_custkey")), entry("c_name", ((Map)g_key).get("c_name")), entry("revenue", (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res5 = new ArrayList<>();
	for (var x : g) {
		_res5.add(x.l.l_extendedprice * (1 - x.l.l_discount));
	}
	return _res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()), entry("c_acctbal", ((Map)g_key).get("c_acctbal")), entry("n_name", ((Map)g_key).get("n_name")), entry("c_address", ((Map)g_key).get("c_address")), entry("c_phone", ((Map)g_key).get("c_phone")), entry("c_comment", ((Map)g_key).get("c_comment"))));
	}
	return _res0;
}}).get();
	System.out.println(result);
	}
}
