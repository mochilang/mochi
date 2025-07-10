import java.util.*;
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("id", 1);put("name", "Alice");}}, new LinkedHashMap<String,Object>(){{put("id", 2);put("name", "Bob");}}));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Integer>(){{put("id", 100);put("customerId", 1);}}, new LinkedHashMap<String,Integer>(){{put("id", 101);put("customerId", 2);}}));
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("orderId", 100);put("sku", "a");}}, new LinkedHashMap<String,Object>(){{put("orderId", 101);put("sku", "b");}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			for (var i : items) {
				if (!(Objects.equals(((Map)o).get("id"), ((Map)i).get("orderId")))) continue;
				_res1.add(new LinkedHashMap<String,Object>(){{put("name", ((Map)c).get("name"));put("sku", ((Map)i).get("sku"));}});
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Multi Join ---");
	for (var r : result) {
		System.out.println(((Map)r).get("name") + " " + "bought item" + " " + ((Map)r).get("sku"));
	}
	}
}
