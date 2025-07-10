import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("id", 1, "name", "Alice"), Main.<String,Object>mapOf("id", 2, "name", "Bob")));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("id", 100, "customerId", 1), Main.<String,Integer>mapOf("id", 101, "customerId", 2)));
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("orderId", 100, "sku", "a"), Main.<String,Object>mapOf("orderId", 101, "sku", "b")));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			for (var i : items) {
				if (!(Objects.equals(((Map)o).get("id"), ((Map)i).get("orderId")))) continue;
				_res1.add(Main.<String,Object>mapOf("name", ((Map)c).get("name"), "sku", ((Map)i).get("sku")));
			}
		}
	}
	return _res1;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Multi Join ---");
	for (var r : result) {
		System.out.println(((Map)r).get("name") + " " + "bought item" + " " + ((Map)r).get("sku"));
	}
	}
}
