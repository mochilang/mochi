import java.util.*;
public class Main {
	static List<Map<String,Object>> products = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<String,Object>(){{put("name", "Laptop");put("price", 1500);}}, new LinkedHashMap<String,Object>(){{put("name", "Smartphone");put("price", 900);}}, new LinkedHashMap<String,Object>(){{put("name", "Tablet");put("price", 600);}}, new LinkedHashMap<String,Object>(){{put("name", "Monitor");put("price", 300);}}, new LinkedHashMap<String,Object>(){{put("name", "Keyboard");put("price", 100);}}, new LinkedHashMap<String,Object>(){{put("name", "Mouse");put("price", 50);}}, new LinkedHashMap<String,Object>(){{put("name", "Headphones");put("price", 200);}}));
	static List<Object> expensive = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var p : products) {
		_res1.add(p);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (var item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
