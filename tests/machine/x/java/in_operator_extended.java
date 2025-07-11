import java.util.*;
public class InOperatorExtended {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
	List<Integer> ys = (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res0 = new ArrayList<>();
	for (var x : xs) {
		if (!(Objects.equals(x % 2, 1))) continue;
		_res0.add(x);
	}
	return _res0;
}}).get();
	System.out.println(ys.contains(1));
	System.out.println(ys.contains(2));
	Map<String,Integer> m = new HashMap<>(mapOfEntries(entry("a", 1)));
	System.out.println(m.containsKey("a"));
	System.out.println(m.containsKey("b"));
	String s = "hello";
	System.out.println(s.contains(String.valueOf("ell")));
	System.out.println(s.contains(String.valueOf("foo")));
	}
}
