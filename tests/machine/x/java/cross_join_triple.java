import java.util.*;
public class Main {
	static List<Integer> nums = java.util.Arrays.asList(1, 2);
	static List<String> letters = java.util.Arrays.asList("A", "B");
	static List<Boolean> bools = java.util.Arrays.asList(true, false);
	static List<Object> combos = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			for (var b : bools) {
				_res0.add(map("n", n, "l", l, "b", b));
			}
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
	System.out.println("--- Cross Join of three lists ---");
	for (var c : combos) {
		System.out.println(((Map)c).get("n") + " " + ((Map)c).get("l") + " " + ((Map)c).get("b"));
	}
	}
}
