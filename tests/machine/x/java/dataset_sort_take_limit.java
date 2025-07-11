import java.util.*;
public class DatasetSortTakeLimit {
	static List<Map<String,Object>> products = new ArrayList<>(Arrays.asList(mapOfEntries(entry("name", "Laptop"), entry("price", 1500)), mapOfEntries(entry("name", "Smartphone"), entry("price", 900)), mapOfEntries(entry("name", "Tablet"), entry("price", 600)), mapOfEntries(entry("name", "Monitor"), entry("price", 300)), mapOfEntries(entry("name", "Keyboard"), entry("price", 100)), mapOfEntries(entry("name", "Mouse"), entry("price", 50)), mapOfEntries(entry("name", "Headphones"), entry("price", 200))));
	static List<Map<String,Object>> expensive = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res1 = new ArrayList<>();
	for (var p : products) {
		_res1.add(p);
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
	System.out.println("--- Top products (excluding most expensive) ---");
	for (Map<String,Object> item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
