import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 1);}}, new LinkedHashMap<>(){{put("id", 102);put("customerId", 2);}}));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			Map<String,Object> _row2 = new HashMap<>();
			_row2.put("o", o);
			_row2.put("c", c);
			Object _key3 = ((Map)c).get("name");
			List<Object> _b4 = _groups1.get(_key3);
			if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
			_b4.add(_row2);
		}
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(new LinkedHashMap<>(){{put("name", g_key);put("count", count(g));}});
	}
	return _res0;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println("--- Orders per customer ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
