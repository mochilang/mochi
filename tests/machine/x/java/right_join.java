import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}, new LinkedHashMap<String,Object>(){{put("id", 3);put("name", "Charlie");}}, new LinkedHashMap<String,Object>(){{put("id", 4);put("name", "Diana");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 2);put("total", 125);}}, new LinkedHashMap<String,Integer>(){{put("id", 102);put("customerId", 1);put("total", 300);}}));
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
			_res3.add(new LinkedHashMap<String,Object>(){{put("customerName", ((Map)c).get("name"));put("order", o);}});
		}
	}
	return _res3;
}}).get();
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
