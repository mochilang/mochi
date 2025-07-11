import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30)), mapOfEntries(entry("name", "Bob"), entry("age", 15)), mapOfEntries(entry("name", "Charlie"), entry("age", 65)), mapOfEntries(entry("name", "Diana"), entry("age", 45))));
	static List<Map<String,Object>> adults = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res1 = new ArrayList<>();
	for (var person : people) {
		if (!(((Number)((Map)person).get("age")).doubleValue() >= 18)) continue;
		_res1.add(mapOfEntries(entry("name", ((Map)person).get("name")), entry("age", ((Map)person).get("age")), entry("is_senior", ((Number)((Map)person).get("age")).doubleValue() >= 60)));
	}
	return _res1;
}}).get();
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (Map<String,Object> person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (((Map)person).get("is_senior") != null ? " (senior)" : ""));
	}
	}
}
