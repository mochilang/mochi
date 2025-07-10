import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}, new LinkedHashMap<>(){{put("id", 3);put("name", "Charlie");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 2);put("total", 125);}}, new LinkedHashMap<>(){{put("id", 102);put("customerId", 1);put("total", 300);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res1.add(new LinkedHashMap<>(){{put("orderId", ((Map)o).get("id"));put("orderCustomerId", ((Map)o).get("customerId"));put("pairedCustomerName", ((Map)c).get("name"));put("orderTotal", ((Map)o).get("total"));}});
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "(customerId:" + " " + ((Map)entry).get("orderCustomerId") + " " + ", total: $" + " " + ((Map)entry).get("orderTotal") + " " + ") paired with" + " " + ((Map)entry).get("pairedCustomerName"));
	}
	}
}
