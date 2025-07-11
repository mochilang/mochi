import java.util.*;
public class Main {
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("cat", "a"), entry("val", 3)), mapOfEntries(entry("cat", "a"), entry("val", 1)), mapOfEntries(entry("cat", "b"), entry("val", 5)), mapOfEntries(entry("cat", "b"), entry("val", 2))));
	static List<Map<String,Object>> grouped = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res6 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups7 = new LinkedHashMap<>();
	for (var i : items) {
		var _row8 = i;
		Object _key9 = ((Map)i).get("cat");
		List<Map<String,Object>> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res6.add(mapOfEntries(entry("cat", g_key), entry("total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(((Map)x).get("val"));
	}
	return _res11;
}}).get()))));
	}
	return _res6;
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
