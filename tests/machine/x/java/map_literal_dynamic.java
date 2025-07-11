import java.util.*;
public class MapLiteralDynamic {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	int x = 3;
	int y = 4;
	Map<String,Integer> m = new HashMap<>(mapOfEntries(entry("a", x), entry("b", y)));
	System.out.println(m.get("a") + " " + m.get("b"));
	}
}
