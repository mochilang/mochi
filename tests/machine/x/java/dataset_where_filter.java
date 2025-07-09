import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("name", "Alice", "age", 30)), new HashMap<>(java.util.Map.of("name", "Bob", "age", 15)), new HashMap<>(java.util.Map.of("name", "Charlie", "age", 65)), new HashMap<>(java.util.Map.of("name", "Diana", "age", 45))));
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var person : people) {
		if (!(((Map)person).get("age") >= 18)) continue;
		_res0.add(new HashMap<>(java.util.Map.of("name", ((Map)person).get("name"), "age", ((Map)person).get("age"), "is_senior", ((Map)person).get("age") >= 60)));
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (var person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (((Map)person).get("is_senior") ? " (senior)" : ""));
	}
	}
}
