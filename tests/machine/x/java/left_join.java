import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = java.util.Arrays.asList(map("id", 1, "name", "Alice"), map("id", 2, "name", "Bob"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("id", 100, "customerId", 1, "total", 250), map("id", 101, "customerId", 3, "total", 80));
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
			_res0.add(map("orderId", ((Map)o).get("id"), "customer", c, "total", ((Map)o).get("total")));
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
	System.out.println("--- Left Join ---");
	for (var entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "customer" + " " + ((Map)entry).get("customer") + " " + "total" + " " + ((Map)entry).get("total"));
	}
	}
}
