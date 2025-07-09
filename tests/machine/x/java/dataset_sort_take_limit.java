import java.util.*;
public class Main {
	static List<Map<Object,Object>> products = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("name", "Laptop", "price", 1500)), new HashMap<>(java.util.Map.of("name", "Smartphone", "price", 900)), new HashMap<>(java.util.Map.of("name", "Tablet", "price", 600)), new HashMap<>(java.util.Map.of("name", "Monitor", "price", 300)), new HashMap<>(java.util.Map.of("name", "Keyboard", "price", 100)), new HashMap<>(java.util.Map.of("name", "Mouse", "price", 50)), new HashMap<>(java.util.Map.of("name", "Headphones", "price", 200))));
	static List<Object> expensive = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var p : products) {
		_res0.add(p);
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (var item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
