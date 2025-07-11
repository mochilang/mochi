import java.util.*;
public class GroupBy {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> people = new ArrayList<>(Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30), entry("city", "Paris")), mapOfEntries(entry("name", "Bob"), entry("age", 15), entry("city", "Hanoi")), mapOfEntries(entry("name", "Charlie"), entry("age", 65), entry("city", "Paris")), mapOfEntries(entry("name", "Diana"), entry("age", 45), entry("city", "Hanoi")), mapOfEntries(entry("name", "Eve"), entry("age", 70), entry("city", "Paris")), mapOfEntries(entry("name", "Frank"), entry("age", 22), entry("city", "Hanoi"))));
	List<Map<String,Object>> stats = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups1 = new LinkedHashMap<>();
	for (var person : people) {
		var _row2 = person;
		Object _key3 = ((Map)person).get("city");
		List<Map<String,Object>> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<Object,List<Map<String,Object>>> __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res0.add(mapOfEntries(entry("city", g_key), entry("count", g.size()), entry("avg_age", (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res5 = new ArrayList<>();
	for (var p : g) {
		_res5.add(((Map)p).get("age"));
	}
	return _res5;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0))));
	}
	return _res0;
}}).get();
	System.out.println("--- People grouped by city ---");
	for (Map<String,Object> s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
