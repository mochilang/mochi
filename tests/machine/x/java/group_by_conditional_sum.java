import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("cat", "a");put("val", 10);put("flag", true);}}, new LinkedHashMap<>(){{put("cat", "a");put("val", 5);put("flag", false);}}, new LinkedHashMap<>(){{put("cat", "b");put("val", 20);put("flag", true);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res7 = new ArrayList<>();
	Map<Object,List<Object>> _groups8 = new LinkedHashMap<>();
	for (var i : items) {
		var _row9 = i;
		Object _key10 = ((Map)i).get("cat");
		List<Object> _b11 = _groups8.get(_key10);
		if (_b11 == null) { _b11 = new ArrayList<>(); _groups8.put(_key10, _b11); }
		_b11.add(_row9);
	}
	for (var __e : _groups8.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res7.add(new LinkedHashMap<>(){{put("cat", g_key);put("share", ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res12 = new ArrayList<>();
	for (var x : g) {
		_res12.add((Boolean.TRUE.equals(((Map)x).get("flag")) ? ((Map)x).get("val") : 0));
	}
	return _res12;
}}).get())).doubleValue() / ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res13 = new ArrayList<>();
	for (var x : g) {
		_res13.add(((Map)x).get("val"));
	}
	return _res13;
}}).get())).doubleValue());}});
	}
	return _res7;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
