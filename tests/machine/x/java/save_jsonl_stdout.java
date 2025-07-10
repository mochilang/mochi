import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(Map.ofEntries(Map.entry("name", "Alice"), Map.entry("age", 30))), new LinkedHashMap<>(Map.ofEntries(Map.entry("name", "Bob"), Map.entry("age", 25)))));
	static void saveJsonl(List<Map<?,?>> list) {
		for (Map<?,?> m : list) {
			List<String> parts = new ArrayList<>();
			for (var e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
			System.out.println("{" + String.join(",", parts) + "}");
		}
	}
	public static void main(String[] args) {
	saveJsonl((List<Map<?,?>>)(List<?>)people);
	}
}
