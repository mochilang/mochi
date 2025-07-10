import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 2), entry("total", 125)), mapOfEntries(entry("id", 102), entry("customerId", 1), entry("total", 300)), mapOfEntries(entry("id", 103), entry("customerId", 4), entry("total", 80))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_res1.add(mapOfEntries(entry("orderId", ((Map)o).get("id")), entry("customerName", ((Map)c).get("name")), entry("total", ((Map)o).get("total"))));
		}
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
	System.out.println("--- Orders with customer info ---");
	for (Map<String,Object> entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "by" + " " + ((Map)entry).get("customerName") + " " + "- $" + " " + ((Map)entry).get("total"));
	}
	}
}
