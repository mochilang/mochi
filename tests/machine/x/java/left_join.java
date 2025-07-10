import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 3);put("total", 80);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res3 = new ArrayList<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(new LinkedHashMap<>(){{put("orderId", ((Map)o).get("id"));put("customer", c);put("total", ((Map)o).get("total"));}});
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "customer" + " " + ((Map)entry).get("customer") + " " + "total" + " " + ((Map)entry).get("total"));
	}
	}
}
