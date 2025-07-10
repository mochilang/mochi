import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("name", "Alice", "age", 30), Main.<String,Object>mapOf("name", "Bob", "age", 25)));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
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
