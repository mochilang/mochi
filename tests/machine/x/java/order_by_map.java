import java.util.*;
public class Main {
	static List<Map<Object,Integer>> data = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("a", 1);put("b", 2);}}, new LinkedHashMap<>(){{put("a", 1);put("b", 1);}}, new LinkedHashMap<>(){{put("a", 0);put("b", 5);}}));
	static List<Object> sorted = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var x : data) {
		_res1.add(x);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
