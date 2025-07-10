import java.util.*;
public class Main {
	static Map<String,Integer> m = new HashMap<>(mapOfEntries(entry("a", 1), entry("b", 2)));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	for (var k : m.keySet()) {
		System.out.println(k);
	}
	}
}
