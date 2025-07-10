import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("name", "Alice", "age", 30), Main.<String,Object>mapOf("name", "Bob", "age", 15), Main.<String,Object>mapOf("name", "Charlie", "age", 65), Main.<String,Object>mapOf("name", "Diana", "age", 45)));
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var person : people) {
		if (!(((Number)((Map)person).get("age")).doubleValue() >= 18)) continue;
		_res1.add(Main.<String,Object>mapOf("name", ((Map)person).get("name"), "age", ((Map)person).get("age"), "is_senior", ((Number)((Map)person).get("age")).doubleValue() >= 60));
	}
	return _res1;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (var person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (((Map)person).get("is_senior") != null ? " (senior)" : ""));
	}
	}
}
