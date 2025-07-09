import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 1, "name", "Alice")), new HashMap<>(java.util.Map.of("id", 2, "name", "Bob")), new HashMap<>(java.util.Map.of("id", 3, "name", "Charlie"))));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new HashMap<>(java.util.Map.of("id", 101, "customerId", 2, "total", 125)), new HashMap<>(java.util.Map.of("id", 102, "customerId", 1, "total", 300))));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res0.add(new HashMap<>(java.util.Map.of("orderId", ((Map)o).get("id"), "orderCustomerId", ((Map)o).get("customerId"), "pairedCustomerName", ((Map)c).get("name"), "orderTotal", ((Map)o).get("total"))));
		}
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "(customerId:" + " " + ((Map)entry).get("orderCustomerId") + " " + ", total: $" + " " + ((Map)entry).get("orderTotal") + " " + ") paired with" + " " + ((Map)entry).get("pairedCustomerName"));
	}
	}
}
