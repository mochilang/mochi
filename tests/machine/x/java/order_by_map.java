import java.util.*;
public class OrderByMap {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Integer>> data = new ArrayList<>(Arrays.asList(mapOfEntries(entry("a", 1), entry("b", 2)), mapOfEntries(entry("a", 1), entry("b", 1)), mapOfEntries(entry("a", 0), entry("b", 5))));
	List<Map<String,Integer>> sorted = (new java.util.function.Supplier<List<Map<String,Integer>>>(){public List<Map<String,Integer>> get(){
	List<Map<String,Integer>> _res0 = new ArrayList<>();
	for (var x : data) {
		_res0.add(x);
	}
	return _res0;
}}).get();
	System.out.println(sorted);
	}
}
