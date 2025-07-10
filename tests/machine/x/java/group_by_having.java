import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = java.util.Arrays.asList(map("name", "Alice", "city", "Paris"), map("name", "Bob", "city", "Hanoi"), map("name", "Charlie", "city", "Paris"), map("name", "Diana", "city", "Hanoi"), map("name", "Eve", "city", "Paris"), map("name", "Frank", "city", "Hanoi"), map("name", "George", "city", "Paris"));
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
		_res0.add(map("city", g_key, "num", count(g)));
	}
	return _res0;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
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
