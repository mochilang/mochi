import java.util.*;
public class Main {
	static List<Map<String,String>> people = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("city", "Paris")), mapOfEntries(entry("name", "Bob"), entry("city", "Hanoi")), mapOfEntries(entry("name", "Charlie"), entry("city", "Paris")), mapOfEntries(entry("name", "Diana"), entry("city", "Hanoi")), mapOfEntries(entry("name", "Eve"), entry("city", "Paris")), mapOfEntries(entry("name", "Frank"), entry("city", "Hanoi")), mapOfEntries(entry("name", "George"), entry("city", "Paris"))));
	static List<Object> big = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	Map<Object,List<Object>> _groups6 = new LinkedHashMap<>();
	for (var p : people) {
		var _row7 = p;
		Object _key8 = ((Map)p).get("city");
		List<Object> _b9 = _groups6.get(_key8);
		if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
		_b9.add(_row7);
	}
	for (var __e : _groups6.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		if (!(count(g) >= 4)) continue;
		_res5.add(mapOfEntries(entry("city", g_key), entry("num", count(g))));
	}
	return _res5;
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
	static String toJson(Object o) {
		if (o instanceof Map<?,?> m) {
			StringJoiner j = new StringJoiner(",", "{", "}");
			for (var e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + e.getValue());
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
	json(big);
	}
}
