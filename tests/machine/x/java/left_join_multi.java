import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = java.util.Arrays.asList(map("id", 1, "name", "Alice"), map("id", 2, "name", "Bob"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("id", 100, "customerId", 1), map("id", 101, "customerId", 2));
	static List<Map<Object,Object>> items = java.util.Arrays.asList(map("orderId", 100, "sku", "a"));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			List<Object> _tmp1 = new ArrayList<>();
			for (var _it2 : items) {
				var i = _it2;
				if (!(((Map)o).get("id") == ((Map)i).get("orderId"))) continue;
				_tmp1.add(_it2);
			}
			if (_tmp1.isEmpty()) _tmp1.add(null);
			for (var i : _tmp1) {
				_res0.add(map("orderId", ((Map)o).get("id"), "name", ((Map)c).get("name"), "item", i));
			}
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
	System.out.println("--- Left Join Multi ---");
	for (var r : result) {
		System.out.println(((Map)r).get("orderId") + " " + ((Map)r).get("name") + " " + ((Map)r).get("item"));
	}
	}
}
