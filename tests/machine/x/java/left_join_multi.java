import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 2))));
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("orderId", 100), entry("sku", "a"))));
	static List<Map<String,Object>> result = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res3 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Integer)((Map)o).get("customerId")), ((Map)c).get("id")))) continue;
			List<Object> _tmp4 = new ArrayList<>();
			for (var _it5 : items) {
				var i = _it5;
				if (!(Objects.equals(((Integer)((Map)o).get("id")), ((Map)i).get("orderId")))) continue;
				_tmp4.add(_it5);
			}
			if (_tmp4.isEmpty()) _tmp4.add(null);
			for (var i : _tmp4) {
				_res3.add(mapOfEntries(entry("orderId", ((Integer)((Map)o).get("id"))), entry("name", ((Map)c).get("name")), entry("item", i)));
			}
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
	System.out.println("--- Left Join Multi ---");
	for (Map<String,Object> r : result) {
		System.out.println(((Map)r).get("orderId") + " " + ((Map)r).get("name") + " " + ((Map)r).get("item"));
	}
	}
}
