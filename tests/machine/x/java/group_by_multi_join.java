import java.util.*;
public class GroupByMultiJoin {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> nations = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "A")), mapOfEntries(entry("id", 2), entry("name", "B"))));
	List<Map<String,Integer>> suppliers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("nation", 1)), mapOfEntries(entry("id", 2), entry("nation", 2))));
	List<Map<String,Object>> partsupp = new ArrayList<>(Arrays.asList(mapOfEntries(entry("part", 100), entry("supplier", 1), entry("cost", 10.000000), entry("qty", 2)), mapOfEntries(entry("part", 100), entry("supplier", 2), entry("cost", 20.000000), entry("qty", 1)), mapOfEntries(entry("part", 200), entry("supplier", 1), entry("cost", 5.000000), entry("qty", 3))));
	List<Map<String,Object>> filtered = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	for (var ps : partsupp) {
		for (var s : suppliers) {
			if (!(Objects.equals(((Map)s).get("id"), ((Map)ps).get("supplier")))) continue;
			for (var n : nations) {
				if (!(Objects.equals(((Map)n).get("id"), ((Map)s).get("nation")))) continue;
				if (!(Objects.equals(((Map)n).get("name"), "A"))) continue;
				_res0.add(mapOfEntries(entry("part", ((Map)ps).get("part")), entry("value", ((Number)((Map)ps).get("cost")).doubleValue() * ((Number)((Map)ps).get("qty")).doubleValue())));
			}
		}
	}
	return _res0;
}}).get();
	List<Map<String,Object>> grouped = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res1 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups2 = new LinkedHashMap<>();
	for (var x : filtered) {
		var _row3 = x;
		Object _key4 = ((Map)x).get("part");
		List<Map<String,Object>> _b5 = _groups2.get(_key4);
		if (_b5 == null) { _b5 = new ArrayList<>(); _groups2.put(_key4, _b5); }
		_b5.add(_row3);
	}
	for (Map.Entry<Object,List<Map<String,Object>>> __e : _groups2.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res1.add(mapOfEntries(entry("part", g_key), entry("total", (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res6 = new ArrayList<>();
	for (var r : g) {
		_res6.add(((Map)r).get("value"));
	}
	return _res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum())));
	}
	return _res1;
}}).get();
	System.out.println(grouped);
	}
}
