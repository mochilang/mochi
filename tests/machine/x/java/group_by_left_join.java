import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = java.util.Arrays.asList(map("id", 1, "name", "Alice"), map("id", 2, "name", "Bob"), map("id", 3, "name", "Charlie"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("id", 100, "customerId", 1), map("id", 101, "customerId", 1), map("id", 102, "customerId", 2));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp2 = new ArrayList<>();
		for (var _it3 : orders) {
			var o = _it3;
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			_tmp2.add(_it3);
		}
		if (_tmp2.isEmpty()) _tmp2.add(null);
		for (var o : _tmp2) {
			Map<String,Object> _row4 = new HashMap<>();
			_row4.put("c", c);
			_row4.put("o", o);
			Object _key5 = ((Map)c).get("name");
			List<Object> _b6 = _groups1.get(_key5);
			if (_b6 == null) { _b6 = new ArrayList<>(); _groups1.put(_key5, _b6); }
			_b6.add(_row4);
		}
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(map("name", g_key, "count", count((new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res7 = new ArrayList<>();
	for (var r : g) {
		if (!(Boolean.TRUE.equals(((Map)r).get("o")))) continue;
		_res7.add(r);
	}
	return _res7;
}}).get())));
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
	public static void main(String[] args) {
	System.out.println("--- Group Left Join ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
