import java.util.*;
public class MapMembership {
	static Map<String,Integer> m = mapOfEntries(entry("a", 1), entry("b", 2));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(m.containsKey("a"));
	System.out.println(m.containsKey("c"));
	}
}
