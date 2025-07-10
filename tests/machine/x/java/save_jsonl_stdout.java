import java.util.*;
public class Main {
	static List<Map<Object,Object>> people = java.util.Arrays.asList(map("name", "Alice", "age", 30), map("name", "Bob", "age", 25));
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
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
