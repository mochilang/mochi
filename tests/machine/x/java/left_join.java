import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);put("total", 250);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 3);put("total", 80);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		List<Object> _tmp1 = new ArrayList<>();
		for (var _it2 : customers) {
			var c = _it2;
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			_tmp1.add(_it2);
		}
		if (_tmp1.isEmpty()) _tmp1.add(null);
		for (var c : _tmp1) {
			_res0.add(new LinkedHashMap<>(){{put("orderId", ((Map)o).get("id"));put("customer", c);put("total", ((Map)o).get("total"));}});
		}
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "customer" + " " + ((Map)entry).get("customer") + " " + "total" + " " + ((Map)entry).get("total"));
	}
	}
}
