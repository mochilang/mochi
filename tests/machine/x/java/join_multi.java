import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = java.util.Arrays.asList(map("id", 1, "name", "Alice"), map("id", 2, "name", "Bob"));
	static List<Map<Object,Object>> orders = java.util.Arrays.asList(map("id", 100, "customerId", 1), map("id", 101, "customerId", 2));
	static List<Map<Object,Object>> items = java.util.Arrays.asList(map("orderId", 100, "sku", "a"), map("orderId", 101, "sku", "b"));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			for (var i : items) {
				if (!(((Map)o).get("id") == ((Map)i).get("orderId"))) continue;
				_res0.add(map("name", ((Map)c).get("name"), "sku", ((Map)i).get("sku")));
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
	System.out.println("--- Multi Join ---");
	for (var r : result) {
		System.out.println(((Map)r).get("name") + " " + "bought item" + " " + ((Map)r).get("sku"));
	}
	}
}
