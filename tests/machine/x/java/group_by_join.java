import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 1);}}, new LinkedHashMap<>(){{put("id", 102);put("customerId", 2);}}));
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
		_res5.add(new LinkedHashMap<>(){{put("name", g_key);put("count", count(g));}});
	}
	return _res5;
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
