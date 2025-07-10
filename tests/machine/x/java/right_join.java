import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("id", 1, "name", "Alice"), Main.<String,Object>mapOf("id", 2, "name", "Bob"), Main.<String,Object>mapOf("id", 3, "name", "Charlie"), Main.<String,Object>mapOf("id", 4, "name", "Diana")));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("id", 100, "customerId", 1, "total", 250), Main.<String,Integer>mapOf("id", 101, "customerId", 2, "total", 125), Main.<String,Integer>mapOf("id", 102, "customerId", 1, "total", 300)));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res3 = new ArrayList<>();
	for (var c : customers) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : orders) {
			var o = _it5;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var o : _tmp4) {
			_res3.add(Main.<String,Object>mapOf("customerName", ((Map)c).get("name"), "order", o));
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
	System.out.println("--- Right Join using syntax ---");
	for (var entry : result) {
		if (((Map)entry).get("order") != null) {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has order" + " " + ((Map)((Map)entry).get("order")).get("id") + " " + "- $" + " " + ((Map)((Map)entry).get("order")).get("total"));
		}
		else {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has no orders");
		}
	}
	}
}
