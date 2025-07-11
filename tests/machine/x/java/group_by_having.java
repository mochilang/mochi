import java.util.*;
public class GroupByHaving {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	static String toJson(Object o) {
		if (o instanceof Map<?,?> m) {
			StringJoiner j = new StringJoiner(",", "{", "}");
			for (Map.Entry<?,?> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + e.getValue());
			return j.toString();
		} else if (o instanceof Collection<?> c) {
			StringJoiner j = new StringJoiner(",", "[", "]");
			for (var x : c) j.add(toJson(x));
			return j.toString();
		} else if (o instanceof String s) {
			return "\"" + s + "\"";
		}
		return String.valueOf(o);
	}
	static void json(Object o) { System.out.println(toJson(o)); }
	public static void main(String[] args) {
	List<Map<String,String>> people = new ArrayList<>(Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("city", "Paris")), mapOfEntries(entry("name", "Bob"), entry("city", "Hanoi")), mapOfEntries(entry("name", "Charlie"), entry("city", "Paris")), mapOfEntries(entry("name", "Diana"), entry("city", "Hanoi")), mapOfEntries(entry("name", "Eve"), entry("city", "Paris")), mapOfEntries(entry("name", "Frank"), entry("city", "Hanoi")), mapOfEntries(entry("name", "George"), entry("city", "Paris"))));
	List<Map<String,Object>> big = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<String,List<Map<String,String>>> _groups1 = new LinkedHashMap<>();
	for (var p : people) {
		var _row2 = p;
		String _key3 = ((Map)p).get("city");
		List<Map<String,String>> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<String,List<Map<String,String>>> __e : _groups1.entrySet()) {
		String g_key = __e.getKey();
		List<Map<String,String>> g = __e.getValue();
		if (!(g.size() >= 4)) continue;
		_res0.add(mapOfEntries(entry("city", g_key), entry("num", g.size())));
	}
	return _res0;
}}).get();
	json(big);
	}
}
