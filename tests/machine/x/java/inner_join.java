import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = java.util.Arrays.asList(map("id", 1, "name", "Alice"), map("id", 2, "name", "Bob"), map("id", 3, "name", "Charlie"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("id", 100, "customerId", 1, "total", 250), map("id", 101, "customerId", 2, "total", 125), map("id", 102, "customerId", 1, "total", 300), map("id", 103, "customerId", 4, "total", 80));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			_res0.add(map("orderId", ((Map)o).get("id"), "customerName", ((Map)c).get("name"), "total", ((Map)o).get("total")));
		}
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Orders with customer info ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "by" + " " + ((Map)entry).get("customerName") + " " + "- $" + " " + ((Map)entry).get("total"));
	}
	}
}
