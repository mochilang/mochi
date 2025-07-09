import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("name", "Alice", "age", 30, "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Bob", "age", 15, "city", "Hanoi")), new HashMap<>(java.util.Map.of("name", "Charlie", "age", 65, "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Diana", "age", 45, "city", "Hanoi")), new HashMap<>(java.util.Map.of("name", "Eve", "age", 70, "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Frank", "age", 22, "city", "Hanoi"))));
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
		_res0.add(new HashMap<>(java.util.Map.of("city", g_key, "count", count(g), "avg_age", avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	for (var p : g) {
		_res5.add(((Map)p).get("age"));
	}
	return _res5;
}}).get()))));
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
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
