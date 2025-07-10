import java.util.*;
public class Main {
	static Map<String,Map<String,Integer>> data = new HashMap<>(mapOfEntries(entry("outer", mapOfEntries(entry("inner", 1)))));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	((Map)data.get("outer")).put("inner", 2);
	System.out.println(((Map)data.get("outer")).get("inner"));
	}
}
