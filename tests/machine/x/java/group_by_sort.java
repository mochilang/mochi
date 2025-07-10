import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("cat", "a");put("val", 3);}}, new LinkedHashMap<>(){{put("cat", "a");put("val", 1);}}, new LinkedHashMap<>(){{put("cat", "b");put("val", 5);}}, new LinkedHashMap<>(){{put("cat", "b");put("val", 2);}}));
	static List<Object> grouped = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	Map<Object,List<Object>> _groups7 = new LinkedHashMap<>();
	for (var i : items) {
		var _row8 = i;
		Object _key9 = ((Map)i).get("cat");
		List<Object> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res6.add(new LinkedHashMap<>(){{put("cat", g_key);put("total", sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(((Map)x).get("val"));
	}
	return _res11;
}}).get()));}});
	}
	return _res6;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(grouped);
	}
}
