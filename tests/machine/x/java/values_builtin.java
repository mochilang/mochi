import java.util.*;
public class ValuesBuiltin {
	static Map<String,Integer> m = mapOfEntries(entry("a", 1), entry("b", 2), entry("c", 3));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(new ArrayList<>(m.values()));
	}
}
