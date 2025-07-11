import java.util.*;
public class CrossJoinFilter {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
	List<Map<Object,Object>> pairs = (new java.util.function.Supplier<List<Map<Object,Object>>>(){public List<Map<Object,Object>> get(){
	List<Map<Object,Object>> _res0 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(Objects.equals(n % 2, 0))) continue;
			_res0.add(mapOfEntries(entry("n", n), entry("l", l)));
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Even pairs ---");
	for (Map<Object,Object> p : pairs) {
		System.out.println(((Map)p).get("n") + " " + ((Map)p).get("l"));
	}
	}
}
