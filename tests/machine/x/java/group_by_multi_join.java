import java.util.*;
public class Main {
	static List<Map<Object,Object>> nations = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "A");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "B");}}));
	static List<Map<Object,Integer>> suppliers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("nation", 1);}}, new LinkedHashMap<>(){{put("id", 2);put("nation", 2);}}));
	static List<Map<Object,Object>> partsupp = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("part", 100);put("supplier", 1);put("cost", 10.000000);put("qty", 2);}}, new LinkedHashMap<>(){{put("part", 100);put("supplier", 2);put("cost", 20.000000);put("qty", 1);}}, new LinkedHashMap<>(){{put("part", 200);put("supplier", 1);put("cost", 5.000000);put("qty", 3);}}));
	static List<Object> filtered = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(((Map)s).get("id") == ((Map)ps).get("supplier"))) continue;
			for (var n : nations) {
				if (!(((Map)n).get("id") == ((Map)s).get("nation"))) continue;
				if (!(((Map)n).get("name") == "A")) continue;
				_res0.add(new LinkedHashMap<>(){{put("part", ((Map)ps).get("part"));put("value", ((Number)((Map)ps).get("cost")).doubleValue() * ((Number)((Map)ps).get("qty")).doubleValue());}});
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
		_res1.add(new LinkedHashMap<>(){{put("part", g_key);put("total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	for (var r : g) {
		_res6.add(((Map)r).get("value"));
	}
	return _res6;
}}).get()));}});
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
