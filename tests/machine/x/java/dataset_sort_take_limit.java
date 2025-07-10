import java.util.*;
public class Main {
	static List<Map<String,Object>> products = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("name", "Laptop", "price", 1500), Main.<String,Object>mapOf("name", "Smartphone", "price", 900), Main.<String,Object>mapOf("name", "Tablet", "price", 600), Main.<String,Object>mapOf("name", "Monitor", "price", 300), Main.<String,Object>mapOf("name", "Keyboard", "price", 100), Main.<String,Object>mapOf("name", "Mouse", "price", 50), Main.<String,Object>mapOf("name", "Headphones", "price", 200)));
	static List<Object> expensive = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var p : products) {
		_res1.add(p);
	}
	return _res1;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (var item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
