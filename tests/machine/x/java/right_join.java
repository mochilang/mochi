import java.util.*;
public class RightJoin {
	static List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie")), mapOfEntries(entry("id", 4), entry("name", "Diana"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 2), entry("total", 125)), mapOfEntries(entry("id", 102), entry("customerId", 1), entry("total", 300))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res3 = new ArrayList<>();
	for (var c : customers) {
		List<Map<String,Integer>> _tmp4 = new ArrayList<>();
		for (var _it5 : orders) {
			var o = _it5;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var o : _tmp4) {
			_res3.add(mapOfEntries(entry("customerName", ((Map)c).get("name")), entry("order", o)));
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
	System.out.println("--- Right Join using syntax ---");
	for (Map<String,Object> entry : result) {
		if (((Map)entry).get("order") != null) {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has order" + " " + ((Map)((Map)entry).get("order")).get("id") + " " + "- $" + " " + ((Map)((Map)entry).get("order")).get("total"));
		}
		else {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has no orders");
		}
	}
	}
}
