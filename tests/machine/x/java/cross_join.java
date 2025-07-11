import java.util.*;
public class CrossJoin {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie"))));
	List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 2), entry("total", 125)), mapOfEntries(entry("id", 102), entry("customerId", 1), entry("total", 300))));
	List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res0.add(mapOfEntries(entry("orderId", ((Map)o).get("id")), entry("orderCustomerId", ((Map)o).get("customerId")), entry("pairedCustomerName", ((Map)c).get("name")), entry("orderTotal", ((Map)o).get("total"))));
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (Map<String,Object> entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "(customerId:" + " " + ((Map)entry).get("orderCustomerId") + " " + ", total: $" + " " + ((Map)entry).get("orderTotal") + " " + ") paired with" + " " + ((Map)entry).get("pairedCustomerName"));
	}
	}
}
