import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("id", 1, "name", "Alice"), Main.<String,Object>mapOf("id", 2, "name", "Bob")));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("id", 100, "customerId", 1), Main.<String,Integer>mapOf("id", 101, "customerId", 2)));
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("orderId", 100, "sku", "a")));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res3 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			List<Object> _tmp4 = new ArrayList<>();
			for (var _it5 : items) {
				var i = _it5;
				if (!(Objects.equals(((Map)o).get("id"), ((Map)i).get("orderId")))) continue;
				_tmp4.add(_it5);
			}
			if (_tmp4.isEmpty()) _tmp4.add(null);
			for (var i : _tmp4) {
				_res3.add(Main.<String,Object>mapOf("orderId", ((Map)o).get("id"), "name", ((Map)c).get("name"), "item", i));
			}
		}
	}
	return _res3;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Left Join Multi ---");
	for (var r : result) {
		System.out.println(((Map)r).get("orderId") + " " + ((Map)r).get("name") + " " + ((Map)r).get("item"));
	}
	}
}
