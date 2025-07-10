import java.util.*;
public class Main {
	static List<Map<String,Object>> people = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30)), mapOfEntries(entry("name", "Bob"), entry("age", 25))));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
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
