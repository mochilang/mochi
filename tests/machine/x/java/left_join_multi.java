import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 2);}}));
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("orderId", 100);put("sku", "a");}}));
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
				_res3.add(new LinkedHashMap<String,Object>(){{put("orderId", ((Map)o).get("id"));put("name", ((Map)c).get("name"));put("item", i);}});
			}
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join Multi ---");
	for (var r : result) {
		System.out.println(((Map)r).get("orderId") + " " + ((Map)r).get("name") + " " + ((Map)r).get("item"));
	}
	}
}
