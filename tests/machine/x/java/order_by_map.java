import java.util.*;
public class Main {
	static List<Map<String,Integer>> data = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("a", 1), entry("b", 2)), mapOfEntries(entry("a", 1), entry("b", 1)), mapOfEntries(entry("a", 0), entry("b", 5))));
	static List<Object> sorted = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var x : data) {
		_res1.add(x);
	}
	return _res1;
}}).get();
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
