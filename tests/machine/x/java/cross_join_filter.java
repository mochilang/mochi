import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(1, 2, 3));
	static List<String> letters = new ArrayList<>(java.util.Arrays.asList("A", "B"));
	static List<Object> pairs = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(Objects.equals(n % 2, 0))) continue;
			_res1.add(mapOfEntries(entry("n", n), entry("l", l)));
		}
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
	System.out.println("--- Even pairs ---");
	for (var p : pairs) {
		System.out.println(((Map)p).get("n") + " " + ((Map)p).get("l"));
	}
	}
}
