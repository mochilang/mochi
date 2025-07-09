import java.util.*;
public class Main {
	static List<Map<Object,Integer>> data = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("a", 1, "b", 2)), new HashMap<>(java.util.Map.of("a", 1, "b", 1)), new HashMap<>(java.util.Map.of("a", 0, "b", 5))));
	static List<Object> sorted = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var x : data) {
		_res0.add(x);
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
