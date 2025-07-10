import java.util.*;
public class Main {
	static List<Map<String,Object>> nations = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "A");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "B");}}));
	static List<Map<String,Integer>> suppliers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 1);put("nation", 1);}}, new LinkedHashMap<String,Integer>(){{put("id", 2);put("nation", 2);}}));
	static List<Map<String,Object>> partsupp = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("part", 100);put("supplier", 1);put("cost", 10.000000);put("qty", 2);}}, new LinkedHashMap<String,Object>(){{put("part", 100);put("supplier", 2);put("cost", 20.000000);put("qty", 1);}}, new LinkedHashMap<String,Object>(){{put("part", 200);put("supplier", 1);put("cost", 5.000000);put("qty", 3);}}));
	static List<Object> filtered = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res7 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(Objects.equals(((Map)s).get("id"), ((Map)ps).get("supplier")))) continue;
			for (var n : nations) {
				if (!(Objects.equals(((Map)n).get("id"), ((Map)s).get("nation")))) continue;
				if (!(Objects.equals(((Map)n).get("name"), "A"))) continue;
				_res7.add(new LinkedHashMap<String,Object>(){{put("part", ((Map)ps).get("part"));put("value", ((Number)((Map)ps).get("cost")).doubleValue() * ((Number)((Map)ps).get("qty")).doubleValue());}});
			}
		}
	}
	return _res7;
}}).get();
	static List<Object> grouped = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res8 = new ArrayList<>();
	Map<Object,List<Object>> _groups9 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row10 = x;
		Object _key11 = ((Map)x).get("part");
		List<Object> _b12 = _groups9.get(_key11);
		if (_b12 == null) { _b12 = new ArrayList<>(); _groups9.put(_key11, _b12); }
		_b12.add(_row10);
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res8.add(new LinkedHashMap<String,Object>(){{put("part", g_key);put("total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res13 = new ArrayList<>();
	for (var r : g) {
		_res13.add(((Map)r).get("value"));
	}
	return _res13;
}}).get()));}});
	}
	return _res8;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
