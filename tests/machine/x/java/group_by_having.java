import java.util.*;
public class Main {
	static List<Map<String,String>> people = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,String>(){{put("name", "Alice");put("city", "Paris");}}, new LinkedHashMap<String,String>(){{put("name", "Bob");put("city", "Hanoi");}}, new LinkedHashMap<String,String>(){{put("name", "Charlie");put("city", "Paris");}}, new LinkedHashMap<String,String>(){{put("name", "Diana");put("city", "Hanoi");}}, new LinkedHashMap<String,String>(){{put("name", "Eve");put("city", "Paris");}}, new LinkedHashMap<String,String>(){{put("name", "Frank");put("city", "Hanoi");}}, new LinkedHashMap<String,String>(){{put("name", "George");put("city", "Paris");}}));
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
		_res5.add(new LinkedHashMap<String,Object>(){{put("city", g_key);put("num", count(g));}});
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
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
