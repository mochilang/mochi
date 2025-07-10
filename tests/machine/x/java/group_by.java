import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = java.util.Arrays.asList(map("name", "Alice", "age", 30, "city", "Paris"), map("name", "Bob", "age", 15, "city", "Hanoi"), map("name", "Charlie", "age", 65, "city", "Paris"), map("name", "Diana", "age", 45, "city", "Hanoi"), map("name", "Eve", "age", 70, "city", "Paris"), map("name", "Frank", "age", 22, "city", "Hanoi"));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var person : people) {
		var _row2 = person;
		Object _key3 = ((Map)person).get("city");
		List<Object> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(map("city", g_key, "count", count(g), "avg_age", avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	for (var p : g) {
		_res5.add(((Map)p).get("age"));
	}
	return _res5;
}}).get())));
	}
	return _res0;
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
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
