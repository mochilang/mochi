import java.util.*;
public class GroupBy {
	static List<Map<String,Object>> people = new ArrayList<>(Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30), entry("city", "Paris")), mapOfEntries(entry("name", "Bob"), entry("age", 15), entry("city", "Hanoi")), mapOfEntries(entry("name", "Charlie"), entry("age", 65), entry("city", "Paris")), mapOfEntries(entry("name", "Diana"), entry("age", 45), entry("city", "Hanoi")), mapOfEntries(entry("name", "Eve"), entry("age", 70), entry("city", "Paris")), mapOfEntries(entry("name", "Frank"), entry("age", 22), entry("city", "Hanoi"))));
	static List<Map<String,Object>> stats = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res6 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups7 = new LinkedHashMap<>();
	for (var person : people) {
		var _row8 = person;
		Object _key9 = ((Map)person).get("city");
		List<Map<String,Object>> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res6.add(mapOfEntries(entry("city", g_key), entry("count", g.size()), entry("avg_age", (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res11 = new ArrayList<>();
	for (var p : g) {
		_res11.add(((Map)p).get("age"));
	}
	return _res11;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0))));
	}
	return _res6;
}}).get();
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- People grouped by city ---");
	for (Map<String,Object> s : stats) {
		System.out.println(((Map)s).get("city") + " " + ": count =" + " " + ((Map)s).get("count") + " " + ", avg_age =" + " " + ((Map)s).get("avg_age"));
	}
	}
}
