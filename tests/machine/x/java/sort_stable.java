import java.util.*;
public class Main {
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("n", 1), entry("v", "a")), mapOfEntries(entry("n", 1), entry("v", "b")), mapOfEntries(entry("n", 2), entry("v", "c"))));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res1 = new ArrayList<>();
	for (var i : items) {
		_res1.add(((Map)i).get("v"));
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
	System.out.println(result);
	}
}
