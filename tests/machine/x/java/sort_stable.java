import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("n", 1, "v", "a")), new HashMap<>(java.util.Map.of("n", 1, "v", "b")), new HashMap<>(java.util.Map.of("n", 2, "v", "c"))));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	for (var i : items) {
		_res0.add(((Map)i).get("v"));
	}
	return _res0;
}}).get();
	public static void main(String[] args) {
	System.out.println(result);
	}
}
