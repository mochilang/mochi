import java.util.*;
public class SaveJsonlStdout {
	static List<Map<String,Object>> people = new ArrayList<>(Arrays.asList(mapOfEntries(entry("name", "Alice"), entry("age", 30)), mapOfEntries(entry("name", "Bob"), entry("age", 25))));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	static Map<String,Object> asMap(Object o) {
		if (o instanceof Map<?,?> mm) {
			LinkedHashMap<String,Object> m = new LinkedHashMap<>();
			for (var e : mm.entrySet()) m.put(String.valueOf(e.getKey()), e.getValue());
			return m;
		}
		LinkedHashMap<String,Object> m = new LinkedHashMap<>();
		for (var f : o.getClass().getDeclaredFields()) { try { f.setAccessible(true); m.put(f.getName(), f.get(o)); } catch (Exception e) { throw new RuntimeException(e); } }
		return m;
	}
	static void saveJsonl(List<?> list) {
		for (Object obj : list) {
			Map<String,Object> m = asMap(obj);
			List<String> parts = new ArrayList<>();
			for (var e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
			System.out.println("{" + String.join(",", parts) + "}");
		}
	}
	public static void main(String[] args) {
	saveJsonl((List<?>)people);
	}
}
