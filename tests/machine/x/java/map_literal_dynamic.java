import java.util.*;
public class Main {
	static int x = 3;
	static int y = 4;
	static Map<String,Object> m = mapOfEntries(entry("a", x), entry("b", y));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(m.get("a") + " " + m.get("b"));
	}
}
