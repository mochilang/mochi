import java.util.*;
public class JoinMulti {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob"))));
	List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 2))));
	List<Map<String,Object>> items = new ArrayList<>(Arrays.asList(mapOfEntries(entry("orderId", 100), entry("sku", "a")), mapOfEntries(entry("orderId", 101), entry("sku", "b"))));
	List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			for (var i : items) {
				if (!(Objects.equals(((Map)o).get("id"), ((Map)i).get("orderId")))) continue;
				_res0.add(mapOfEntries(entry("name", ((Map)c).get("name")), entry("sku", ((Map)i).get("sku"))));
			}
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Multi Join ---");
	for (Map<String,Object> r : result) {
		System.out.println(((Map)r).get("name") + " " + "bought item" + " " + ((Map)r).get("sku"));
	}
	}
}
