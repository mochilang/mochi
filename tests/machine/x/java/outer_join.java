import java.util.*;
public class OuterJoin {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie")), mapOfEntries(entry("id", 4), entry("name", "Diana"))));
	List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1), entry("total", 250)), mapOfEntries(entry("id", 101), entry("customerId", 2), entry("total", 125)), mapOfEntries(entry("id", 102), entry("customerId", 1), entry("total", 300)), mapOfEntries(entry("id", 103), entry("customerId", 5), entry("total", 80))));
	List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	java.util.Set<Object> _matched = new java.util.HashSet<>();
	for (var o : orders) {
		List<Map<String,Object>> _tmp1 = new ArrayList<>();
		for (var _it2 : customers) {
			var c = _it2;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp1.add(_it2);
			_matched.add(_it2);
		}
		if (_tmp1.isEmpty()) _tmp1.add(null);
		for (var c : _tmp1) {
			_res0.add(mapOfEntries(entry("order", o), entry("customer", c)));
		}
	}
	for (var c : customers) {
		if (!_matched.contains(c)) {
			Object o = null;
			_res0.add(mapOfEntries(entry("order", o), entry("customer", c)));
		}
	}
	return _res0;
}}).get();
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
