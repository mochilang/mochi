import java.util.*;
public class MapInOperator {
	static Map<Integer,String> m = mapOfEntries(entry(1, "a"), entry(2, "b"));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(m.containsKey(1));
	System.out.println(m.containsKey(3));
	}
}
