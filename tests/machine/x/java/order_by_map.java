import java.util.*;
public class Main {
	static List<Map<String,Integer>> data = new ArrayList<>(java.util.Arrays.asList(Main.<String,Integer>mapOf("a", 1, "b", 2), Main.<String,Integer>mapOf("a", 1, "b", 1), Main.<String,Integer>mapOf("a", 0, "b", 5)));
	static List<Object> sorted = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var x : data) {
		_res1.add(x);
	}
	return _res1;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
