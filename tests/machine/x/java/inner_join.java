import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}, new LinkedHashMap<String,Object>(){{put("id", 3);put("name", "Charlie");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 2);put("total", 125);}}, new LinkedHashMap<String,Integer>(){{put("id", 102);put("customerId", 1);put("total", 300);}}, new LinkedHashMap<String,Integer>(){{put("id", 103);put("customerId", 4);put("total", 80);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_res1.add(new LinkedHashMap<String,Object>(){{put("orderId", ((Map)o).get("id"));put("customerName", ((Map)c).get("name"));put("total", ((Map)o).get("total"));}});
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Orders with customer info ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "by" + " " + ((Map)entry).get("customerName") + " " + "- $" + " " + ((Map)entry).get("total"));
	}
	}
}
