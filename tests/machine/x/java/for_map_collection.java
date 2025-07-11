import java.util.*;
public class ForMapCollection {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	Map<String,Integer> m = new HashMap<>(mapOfEntries(entry("a", 1), entry("b", 2)));
	for (String k : m.keySet()) {
		System.out.println(k);
	}
	}
}
