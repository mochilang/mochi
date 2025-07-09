import java.util.*;
public class Main {
	static List<Map<Object,String>> people = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("name", "Alice", "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Bob", "city", "Hanoi")), new HashMap<>(java.util.Map.of("name", "Charlie", "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Diana", "city", "Hanoi")), new HashMap<>(java.util.Map.of("name", "Eve", "city", "Paris")), new HashMap<>(java.util.Map.of("name", "Frank", "city", "Hanoi")), new HashMap<>(java.util.Map.of("name", "George", "city", "Paris"))));
	static List<Object> big = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var p : people) {
		var _row2 = p;
		Object _key3 = ((Map)p).get("city");
		List<Object> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		if (!(count(g) >= 4)) continue;
		_res0.add(new HashMap<>(java.util.Map.of("city", g_key, "num", count(g))));
	}
	return _res0;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	json(big);
	}
}
