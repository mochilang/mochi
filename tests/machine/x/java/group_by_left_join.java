import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}, new LinkedHashMap<String,Object>(){{put("id", 3);put("name", "Charlie");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 1);}}, new LinkedHashMap<String,Integer>(){{put("id", 102);put("customerId", 2);}}));
	static List<Object> stats = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res8 = new ArrayList<>();
	Map<Object,List<Object>> _groups9 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp10 = new ArrayList<>();
		for (var _it11 : orders) {
			var o = _it11;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp10.add(_it11);
		}
		if (_tmp10.isEmpty()) _tmp10.add(null);
		for (var o : _tmp10) {
			Map<String,Object> _row12 = new HashMap<>();
			_row12.put("c", c);
			_row12.put("o", o);
			Object _key13 = ((Map)c).get("name");
			List<Object> _b14 = _groups9.get(_key13);
			if (_b14 == null) { _b14 = new ArrayList<>(); _groups9.put(_key13, _b14); }
			_b14.add(_row12);
		}
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res8.add(new LinkedHashMap<String,Object>(){{put("name", g_key);put("count", count((new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res15 = new ArrayList<>();
	for (var r : g) {
		if (!(((Map)r).get("o") != null)) continue;
		_res15.add(r);
	}
	return _res15;
}}).get()));}});
	}
	return _res8;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println("--- Group Left Join ---");
	for (var s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
