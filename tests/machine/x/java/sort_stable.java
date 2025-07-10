import java.util.*;
public class Main {
	static List<Map<String,Object>> items = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("n", 1, "v", "a"), Main.<String,Object>mapOf("n", 1, "v", "b"), Main.<String,Object>mapOf("n", 2, "v", "c")));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var i : items) {
		_res1.add(((Map)i).get("v"));
	}
	return _res1;
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
