import java.util.*;
public class Main {
	static List<Map<Object,Object>> data = java.util.Arrays.asList(map("a", 1, "b", 2), map("a", 1, "b", 1), map("a", 0, "b", 5));
	static List<Object> sorted = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var x : data) {
		_res0.add(x);
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
