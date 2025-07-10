import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}, new LinkedHashMap<String,Object>(){{put("id", 3);put("name", "Charlie");}}, new LinkedHashMap<String,Object>(){{put("id", 4);put("name", "Diana");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 2);put("total", 125);}}, new LinkedHashMap<String,Integer>(){{put("id", 102);put("customerId", 1);put("total", 300);}}, new LinkedHashMap<String,Integer>(){{put("id", 103);put("customerId", 5);put("total", 80);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res3 = new ArrayList<>();
	java.util.Set<Object> _matched = new java.util.HashSet<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
			_matched.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(new LinkedHashMap<String,Object>(){{put("order", o);put("customer", c);}});
		}
	}
	for (var c : customers) {
		if (!_matched.contains(c)) {
			Object o = null;
			_res3.add(new LinkedHashMap<String,Object>(){{put("order", o);put("customer", c);}});
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Outer Join using syntax ---");
	for (var row : result) {
		if (((Map)row).get("order") != null) {
			if (((Map)row).get("customer") != null) {
				System.out.println("Order" + " " + ((Map)((Map)row).get("order")).get("id") + " " + "by" + " " + ((Map)((Map)row).get("customer")).get("name") + " " + "- $" + " " + ((Map)((Map)row).get("order")).get("total"));
			}
			else {
				System.out.println("Order" + " " + ((Map)((Map)row).get("order")).get("id") + " " + "by" + " " + "Unknown" + " " + "- $" + " " + ((Map)((Map)row).get("order")).get("total"));
			}
		}
		else {
			System.out.println("Customer" + " " + ((Map)((Map)row).get("customer")).get("name") + " " + "has no orders");
		}
	}
	}
}
