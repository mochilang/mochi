import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("name", "Alice");put("age", 30);put("city", "Paris");}}, new LinkedHashMap<>(){{put("name", "Bob");put("age", 15);put("city", "Hanoi");}}, new LinkedHashMap<>(){{put("name", "Charlie");put("age", 65);put("city", "Paris");}}, new LinkedHashMap<>(){{put("name", "Diana");put("age", 45);put("city", "Hanoi");}}, new LinkedHashMap<>(){{put("name", "Eve");put("age", 70);put("city", "Paris");}}, new LinkedHashMap<>(){{put("name", "Frank");put("age", 22);put("city", "Hanoi");}}));
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
		_res6.add(new LinkedHashMap<>(){{put("city", g_key);put("count", count(g));put("avg_age", avg((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(((Map)p).get("age"));
	}
	return _res11;
}}).get()));}});
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
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
