import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 1, "name", "Alice")), new HashMap<>(java.util.Map.of("id", 2, "name", "Bob"))));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("id", 100, "customerId", 1)), new HashMap<>(java.util.Map.of("id", 101, "customerId", 2))));
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("orderId", 100, "sku", "a")), new HashMap<>(java.util.Map.of("orderId", 101, "sku", "b"))));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			for (var i : items) {
				if (!(((Map)o).get("id") == ((Map)i).get("orderId"))) continue;
				_res0.add(new HashMap<>(java.util.Map.of("name", ((Map)c).get("name"), "sku", ((Map)i).get("sku"))));
			}
		}
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Multi Join ---");
	for (var r : result) {
		System.out.println(((Map)r).get("name") + " " + "bought item" + " " + ((Map)r).get("sku"));
	}
	}
}
