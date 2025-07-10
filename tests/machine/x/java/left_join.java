import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 3), entry("total", 80))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res3 = new ArrayList<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(mapOfEntries(entry("orderId", ((Map)o).get("id")), entry("customer", c), entry("total", ((Map)o).get("total"))));
		}
	}
	return _res3;
}}).get();
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Left Join ---");
	for (Map<String,Object> entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "customer" + " " + ((Map)entry).get("customer") + " " + "total" + " " + ((Map)entry).get("total"));
	}
	}
}
