import java.util.*;
public class Main {
	static List<Map<String,Object>> nations = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "A")), mapOfEntries(entry("id", 2), entry("name", "B"))));
	static List<Map<String,Integer>> suppliers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("nation", 1)), mapOfEntries(entry("id", 2), entry("nation", 2))));
	static List<Map<String,Object>> partsupp = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("part", 100), entry("supplier", 1), entry("cost", 10.000000), entry("qty", 2)), mapOfEntries(entry("part", 100), entry("supplier", 2), entry("cost", 20.000000), entry("qty", 1)), mapOfEntries(entry("part", 200), entry("supplier", 1), entry("cost", 5.000000), entry("qty", 3))));
	static List<Map<String,Object>> filtered = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res7 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(Objects.equals(((Integer)((Map)s).get("id")), ((Map)ps).get("supplier")))) continue;
			for (var n : nations) {
				if (!(Objects.equals(((Map)n).get("id"), ((Integer)((Map)s).get("nation"))))) continue;
				if (!(Objects.equals(((Map)n).get("name"), "A"))) continue;
				_res7.add(mapOfEntries(entry("part", ((Map)ps).get("part")), entry("value", ((Number)((Map)ps).get("cost")).doubleValue() * ((Number)((Map)ps).get("qty")).doubleValue())));
			}
		}
	}
	return _res7;
}}).get();
	static List<Map<String,Object>> grouped = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res8 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups9 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row10 = x;
		Object _key11 = ((Map)x).get("part");
		List<Map<String,Object>> _b12 = _groups9.get(_key11);
		if (_b12 == null) { _b12 = new ArrayList<>(); _groups9.put(_key11, _b12); }
		_b12.add(_row10);
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res8.add(mapOfEntries(entry("part", g_key), entry("total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res13 = new ArrayList<>();
	for (var r : g) {
		_res13.add(((Map)r).get("value"));
	}
	return _res13;
}}).get()))));
	}
	return _res8;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
