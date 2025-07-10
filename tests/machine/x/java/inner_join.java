import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("id", 1, "name", "Alice"), Main.<String,Object>mapOf("id", 2, "name", "Bob"), Main.<String,Object>mapOf("id", 3, "name", "Charlie")));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("id", 100, "customerId", 1, "total", 250), Main.<String,Integer>mapOf("id", 101, "customerId", 2, "total", 125), Main.<String,Integer>mapOf("id", 102, "customerId", 1, "total", 300), Main.<String,Integer>mapOf("id", 103, "customerId", 4, "total", 80)));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_res1.add(Main.<String,Object>mapOf("orderId", ((Map)o).get("id"), "customerName", ((Map)c).get("name"), "total", ((Map)o).get("total")));
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
	System.out.println("--- Orders with customer info ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "by" + " " + ((Map)entry).get("customerName") + " " + "- $" + " " + ((Map)entry).get("total"));
	}
	}
}
