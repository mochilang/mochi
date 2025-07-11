import java.util.*;
public class InOperatorExtended {
	static List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
	static List<Integer> ys = (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res1 = new ArrayList<>();
	for (var x : xs) {
		if (!(Objects.equals(x % 2, 1))) continue;
		_res1.add(x);
	}
	return _res1;
}}).get();
	static Map<String,Integer> m = mapOfEntries(entry("a", 1));
	static String s = "hello";
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(ys.contains(1));
	System.out.println(ys.contains(2));
	System.out.println(m.containsKey("a"));
	System.out.println(m.containsKey("b"));
	System.out.println(s.contains(String.valueOf("ell")));
	System.out.println(s.contains(String.valueOf("foo")));
	}
}
