import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("id", 1, "name", "Alice"), Main.<String,Object>mapOf("id", 2, "name", "Bob")));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("id", 100, "customerId", 1), Main.<String,Integer>mapOf("id", 101, "customerId", 1), Main.<String,Integer>mapOf("id", 102, "customerId", 2)));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	Map<Object,List<Object>> _groups6 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			Map<String,Object> _row7 = new HashMap<>();
			_row7.put("o", o);
			_row7.put("c", c);
			Object _key8 = ((Map)c).get("name");
			List<Object> _b9 = _groups6.get(_key8);
			if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
			_b9.add(_row7);
		}
	}
	for (var __e : _groups6.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res5.add(Main.<String,Object>mapOf("name", g_key, "count", count(g)));
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Orders per customer ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
