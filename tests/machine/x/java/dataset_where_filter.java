import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = java.util.Arrays.asList(map("name", "Alice", "age", 30), map("name", "Bob", "age", 15), map("name", "Charlie", "age", 65), map("name", "Diana", "age", 45));
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var person : people) {
		if (!(Boolean.TRUE.equals(((Number)((Map)person).get("age")).doubleValue() >= 18))) continue;
		_res0.add(map("name", ((Map)person).get("name"), "age", ((Map)person).get("age"), "is_senior", ((Number)((Map)person).get("age")).doubleValue() >= 60));
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (var person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (Boolean.TRUE.equals(((Map)person).get("is_senior")) ? " (senior)" : ""));
	}
	}
}
