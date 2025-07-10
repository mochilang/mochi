import java.util.*;
public class Main {
	static Map<String,Integer> scores = new HashMap<>(mapOfEntries(entry("alice", 1)));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	scores.put("bob", 2);
	System.out.println(scores.get("bob"));
	}
}
