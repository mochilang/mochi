import java.util.*;
public class Main {
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("cat", "a"), entry("val", 10), entry("flag", true)), mapOfEntries(entry("cat", "a"), entry("val", 5), entry("flag", false)), mapOfEntries(entry("cat", "b"), entry("val", 20), entry("flag", true))));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res7 = new ArrayList<>();
	Map<Object,List<Object>> _groups8 = new LinkedHashMap<>();
	for (var i : items) {
		var _row9 = i;
		Object _key10 = ((Map)i).get("cat");
		List<Object> _b11 = _groups8.get(_key10);
		if (_b11 == null) { _b11 = new ArrayList<>(); _groups8.put(_key10, _b11); }
		_b11.add(_row9);
	}
	for (var __e : _groups8.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res7.add(mapOfEntries(entry("cat", g_key), entry("share", ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res12 = new ArrayList<>();
	for (var x : g) {
		_res12.add((((Map)x).get("flag") != null ? ((Map)x).get("val") : 0));
	}
	return _res12;
}}).get())).doubleValue() / ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res13 = new ArrayList<>();
	for (var x : g) {
		_res13.add(((Map)x).get("val"));
	}
	return _res13;
}}).get())).doubleValue())));
	}
	return _res7;
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
	System.out.println(result);
	}
}
