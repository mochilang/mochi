import java.util.*;
public class GroupByConditionalSum {
	static List<Map<String,Object>> items = new ArrayList<>(Arrays.asList(mapOfEntries(entry("cat", "a"), entry("val", 10), entry("flag", true)), mapOfEntries(entry("cat", "a"), entry("val", 5), entry("flag", false)), mapOfEntries(entry("cat", "b"), entry("val", 20), entry("flag", true))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res7 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups8 = new LinkedHashMap<>();
	for (var i : items) {
		var _row9 = i;
		Object _key10 = ((Map)i).get("cat");
		List<Map<String,Object>> _b11 = _groups8.get(_key10);
		if (_b11 == null) { _b11 = new ArrayList<>(); _groups8.put(_key10, _b11); }
		_b11.add(_row9);
	}
	for (var __e : _groups8.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res7.add(mapOfEntries(entry("cat", g_key), entry("share", ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res12 = new ArrayList<>();
	for (var x : g) {
		_res12.add((((Map)x).get("flag") != null ? ((Map)x).get("val") : 0));
	}
	return _res12;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue() / ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res13 = new ArrayList<>();
	for (var x : g) {
		_res13.add(((Map)x).get("val"));
	}
	return _res13;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue())));
	}
	return _res7;
}}).get();
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
