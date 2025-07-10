import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("name", "Alice");put("age", 30);}}, new LinkedHashMap<>(){{put("name", "Bob");put("age", 15);}}, new LinkedHashMap<>(){{put("name", "Charlie");put("age", 65);}}, new LinkedHashMap<>(){{put("name", "Diana");put("age", 45);}}));
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var person : people) {
		if (!(((Number)((Map)person).get("age")).doubleValue() >= 18)) continue;
		_res0.add(new LinkedHashMap<>(){{put("name", ((Map)person).get("name"));put("age", ((Map)person).get("age"));put("is_senior", ((Number)((Map)person).get("age")).doubleValue() >= 60);}});
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (var person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (Boolean.TRUE.equals(((Map)person).get("is_senior")) ? " (senior)" : ""));
	}
	}
}
