import java.util.*;
public class CrossJoinTriple {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2));
	List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
	List<Boolean> bools = new ArrayList<>(Arrays.asList(true, false));
	List<Map<Object,Object>> combos = (new java.util.function.Supplier<List<Map<Object,Object>>>(){public List<Map<Object,Object>> get(){
	List<Map<Object,Object>> _res0 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			for (var b : bools) {
				_res0.add(mapOfEntries(entry("n", n), entry("l", l), entry("b", b)));
			}
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Cross Join of three lists ---");
	for (Map<Object,Object> c : combos) {
		System.out.println(((Map)c).get("n") + " " + ((Map)c).get("l") + " " + ((Map)c).get("b"));
	}
	}
}
