import java.util.*;
public class Main {
	static List<Map<Object,Object>> products = java.util.Arrays.asList(map("name", "Laptop", "price", 1500), map("name", "Smartphone", "price", 900), map("name", "Tablet", "price", 600), map("name", "Monitor", "price", 300), map("name", "Keyboard", "price", 100), map("name", "Mouse", "price", 50), map("name", "Headphones", "price", 200));
	static List<Object> expensive = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var p : products) {
		_res0.add(p);
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (var item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
