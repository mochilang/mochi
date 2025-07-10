import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("n", 1);put("v", "a");}}, new LinkedHashMap<>(){{put("n", 1);put("v", "b");}}, new LinkedHashMap<>(){{put("n", 2);put("v", "c");}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var i : items) {
		_res1.add(((Map)i).get("v"));
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(result);
	}
}
