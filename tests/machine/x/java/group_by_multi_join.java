import java.util.*;
public class Main {
	static List<Map<Object,Object>> nations = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 1, "name", "A")), new HashMap<>(java.util.Map.of("id", 2, "name", "B"))));
	static List<Map<Object,Integer>> suppliers = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 1, "nation", 1)), new HashMap<>(java.util.Map.of("id", 2, "nation", 2))));
	static List<Map<Object,Object>> partsupp = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("part", 100, "supplier", 1, "cost", 10.000000, "qty", 2)), new HashMap<>(java.util.Map.of("part", 100, "supplier", 2, "cost", 20.000000, "qty", 1)), new HashMap<>(java.util.Map.of("part", 200, "supplier", 1, "cost", 5.000000, "qty", 3))));
	static List<Object> filtered = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(((Map)s).get("id") == ((Map)ps).get("supplier"))) continue;
			for (var n : nations) {
				if (!(((Map)n).get("id") == ((Map)s).get("nation"))) continue;
				if (!(((Map)n).get("name") == "A")) continue;
				_res0.add(new HashMap<>(java.util.Map.of("part", ((Map)ps).get("part"), "value", ((Map)ps).get("cost") * ((Map)ps).get("qty"))));
			}
		}
	}
	return _res0;
}}).get();
	static List<Object> grouped = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	Map<Object,List<Object>> _groups2 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row3 = x;
		Object _key4 = ((Map)x).get("part");
		List<Object> _b5 = _groups2.get(_key4);
		if (_b5 == null) { _b5 = new ArrayList<>(); _groups2.put(_key4, _b5); }
		_b5.add(_row3);
	}
	for (var __e : _groups2.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res1.add(new HashMap<>(java.util.Map.of("part", g_key, "total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	for (var r : g) {
		_res6.add(((Map)r).get("value"));
	}
	return _res6;
}}).get()))));
	}
	return _res1;
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
