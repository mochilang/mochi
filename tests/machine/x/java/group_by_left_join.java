import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 1)), mapOfEntries(entry("id", 102), entry("customerId", 2))));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res8 = new ArrayList<>();
	Map<Object,List<Object>> _groups9 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp10 = new ArrayList<>();
		for (var _it11 : orders) {
			var o = _it11;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp10.add(_it11);
		}
		if (_tmp10.isEmpty()) _tmp10.add(null);
		for (var o : _tmp10) {
			Map<String,Object> _row12 = new HashMap<>();
			_row12.put("c", c);
			_row12.put("o", o);
			Object _key13 = ((Map)c).get("name");
			List<Object> _b14 = _groups9.get(_key13);
			if (_b14 == null) { _b14 = new ArrayList<>(); _groups9.put(_key13, _b14); }
			_b14.add(_row12);
		}
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res8.add(mapOfEntries(entry("name", g_key), entry("count", count((new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res15 = new ArrayList<>();
	for (var r : g) {
		if (!(((Map)r).get("o") != null)) continue;
		_res15.add(r);
	}
	return _res15;
}}).get()))));
	}
	return _res8;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Group Left Join ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
