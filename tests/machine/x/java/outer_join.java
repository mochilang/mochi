import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie")), mapOfEntries(entry("id", 4), entry("name", "Diana"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 2), entry("total", 125)), mapOfEntries(entry("id", 102), entry("customerId", 1), entry("total", 300)), mapOfEntries(entry("id", 103), entry("customerId", 5), entry("total", 80))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res3 = new ArrayList<>();
	java.util.Set<Object> _matched = new java.util.HashSet<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(((Integer)((Map)o).get("customerId")), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
			_matched.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(mapOfEntries(entry("order", o), entry("customer", c)));
		}
	}
	for (var c : customers) {
		if (!_matched.contains(c)) {
			Object o = null;
			_res3.add(mapOfEntries(entry("order", o), entry("customer", c)));
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
	System.out.println("--- Outer Join using syntax ---");
	for (Map<String,Object> row : result) {
		if (((Map)row).get("order") != null) {
			if (((Map)row).get("customer") != null) {
				System.out.println("Order" + " " + ((Map)((Map)row).get("order")).get("id") + " " + "by" + " " + ((Map)((Map)row).get("customer")).get("name") + " " + "- $" + " " + ((Map)((Map)row).get("order")).get("total"));
			}
			else {
				System.out.println("Order" + " " + ((Map)((Map)row).get("order")).get("id") + " " + "by" + " " + "Unknown" + " " + "- $" + " " + ((Map)((Map)row).get("order")).get("total"));
			}
		}
		else {
			System.out.println("Customer" + " " + ((Map)((Map)row).get("customer")).get("name") + " " + "has no orders");
		}
	}
	}
}
