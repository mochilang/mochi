import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(java.util.Arrays.asList(1, 2));
	static List<String> letters = new ArrayList<>(java.util.Arrays.asList("A", "B"));
	static List<Boolean> bools = new ArrayList<>(java.util.Arrays.asList(true, false));
	static List<Object> combos = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var n : nums) {
		for (var l : letters) {
			for (var b : bools) {
				_res1.add(new LinkedHashMap<String,Object>(){{put("n", n);put("l", l);put("b", b);}});
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join of three lists ---");
	for (var c : combos) {
		System.out.println(((Map)c).get("n") + " " + ((Map)c).get("l") + " " + ((Map)c).get("b"));
	}
	}
}
