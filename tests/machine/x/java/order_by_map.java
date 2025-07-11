import java.util.*;
public class Main {
	static List<Map<String,Integer>> data = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("a", 1), entry("b", 2)), mapOfEntries(entry("a", 1), entry("b", 1)), mapOfEntries(entry("a", 0), entry("b", 5))));
	static List<Map<String,Integer>> sorted = (new java.util.function.Supplier<List<Map<String,Integer>>>(){public List<Map<String,Integer>> get(){
	List<Map<String,Integer>> _res1 = new ArrayList<>();
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
