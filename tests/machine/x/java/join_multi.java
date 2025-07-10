import java.util.*;
public class Main {
	static List<Map<Object,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<Object,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<>(){{put("id", 101);put("customerId", 2);}}));
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("orderId", 100);put("sku", "a");}}, new LinkedHashMap<>(){{put("orderId", 101);put("sku", "b");}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(((Map)o).get("customerId") == ((Map)c).get("id"))) continue;
			for (var i : items) {
				if (!(((Map)o).get("id") == ((Map)i).get("orderId"))) continue;
				_res0.add(new LinkedHashMap<>(){{put("name", ((Map)c).get("name"));put("sku", ((Map)i).get("sku"));}});
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
