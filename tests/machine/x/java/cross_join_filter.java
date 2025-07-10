import java.util.*;
public class Main {
	static List<Integer> nums = java.util.Arrays.asList(1, 2, 3);
	static List<String> letters = java.util.Arrays.asList("A", "B");
	static List<Object> pairs = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			if (!(n % 2 == 0)) continue;
			_res0.add(map("n", n, "l", l));
		}
	}
	return _res0;
}}).get();
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Even pairs ---");
	for (var p : pairs) {
		System.out.println(((Map)p).get("n") + " " + ((Map)p).get("l"));
	}
	}
}
