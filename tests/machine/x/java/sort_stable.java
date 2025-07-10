import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = java.util.Arrays.asList(map("n", 1, "v", "a"), map("n", 1, "v", "b"), map("n", 2, "v", "c"));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var i : items) {
		_res0.add(((Map)i).get("v"));
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
