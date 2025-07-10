import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("name", "Alice");put("age", 30);}}, new LinkedHashMap<>(){{put("name", "Bob");put("age", 25);}}));
	static void saveJsonl(List<Map<String,Object>> list) {
		for (Map<String,Object> m : list) {
			List<String> parts = new ArrayList<>();
			for (var e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
			System.out.println("{" + String.join(",", parts) + "}");
		}
	}
	public static void main(String[] args) {
	saveJsonl(people);
	}
}
