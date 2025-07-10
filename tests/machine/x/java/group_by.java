import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30), entry("city", "Paris")), mapOfEntries(entry("name", "Bob"), entry("age", 15), entry("city", "Hanoi")), mapOfEntries(entry("name", "Charlie"), entry("age", 65), entry("city", "Paris")), mapOfEntries(entry("name", "Diana"), entry("age", 45), entry("city", "Hanoi")), mapOfEntries(entry("name", "Eve"), entry("age", 70), entry("city", "Paris")), mapOfEntries(entry("name", "Frank"), entry("age", 22), entry("city", "Hanoi"))));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	Map<Object,List<Object>> _groups7 = new LinkedHashMap<>();
	for (var person : people) {
		var _row8 = person;
		Object _key9 = ((Map)person).get("city");
		List<Object> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res6.add(mapOfEntries(entry("city", g_key), entry("count", count(g)), entry("avg_age", avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(((Map)p).get("age"));
	}
	return _res11;
}}).get()))));
	}
	return _res6;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static double avg(List<? extends Number> v) {
		if (v.isEmpty()) return 0;
		int s = 0;
		for (Number n : v) s += n.intValue();
		return (double)s / v.size();
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
