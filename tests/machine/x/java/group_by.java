import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("name", "Alice", "age", 30, "city", "Paris"), Main.<String,Object>mapOf("name", "Bob", "age", 15, "city", "Hanoi"), Main.<String,Object>mapOf("name", "Charlie", "age", 65, "city", "Paris"), Main.<String,Object>mapOf("name", "Diana", "age", 45, "city", "Hanoi"), Main.<String,Object>mapOf("name", "Eve", "age", 70, "city", "Paris"), Main.<String,Object>mapOf("name", "Frank", "age", 22, "city", "Hanoi")));
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
		_res6.add(Main.<String,Object>mapOf("city", g_key, "count", count(g), "avg_age", avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(((Map)p).get("age"));
	}
	return _res11;
}}).get())));
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
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
