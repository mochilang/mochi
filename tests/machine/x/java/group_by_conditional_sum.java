import java.util.*;
public class GroupByConditionalSum {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> items = new ArrayList<>(Arrays.asList(mapOfEntries(entry("cat", "a"), entry("val", 10), entry("flag", true)), mapOfEntries(entry("cat", "a"), entry("val", 5), entry("flag", false)), mapOfEntries(entry("cat", "b"), entry("val", 20), entry("flag", true))));
	List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups1 = new LinkedHashMap<>();
	for (var i : items) {
		var _row2 = i;
		Object _key3 = ((Map)i).get("cat");
		List<Map<String,Object>> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<Object,List<Map<String,Object>>> __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res0.add(mapOfEntries(entry("cat", g_key), entry("share", ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res5 = new ArrayList<>();
	for (var x : g) {
		_res5.add((((Map)x).get("flag") != null ? ((Map)x).get("val") : 0));
	}
	return _res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue() / ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res6 = new ArrayList<>();
	for (var x : g) {
		_res6.add(((Map)x).get("val"));
	}
	return _res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue())));
	}
	return _res0;
}}).get();
	System.out.println(result);
	}
}
