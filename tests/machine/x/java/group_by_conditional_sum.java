import java.util.*;
public class Main {
	static List<Map<Object,Object>> items = new ArrayList<>(java.util.Arrays.asList(new LinkedHashMap<>(){{put("cat", "a");put("val", 10);put("flag", true);}}, new LinkedHashMap<>(){{put("cat", "a");put("val", 5);put("flag", false);}}, new LinkedHashMap<>(){{put("cat", "b");put("val", 20);put("flag", true);}}));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var i : items) {
		var _row2 = i;
		Object _key3 = ((Map)i).get("cat");
		List<Object> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(new LinkedHashMap<>(){{put("cat", g_key);put("share", ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	for (var x : g) {
		_res5.add((Boolean.TRUE.equals(((Map)x).get("flag")) ? ((Map)x).get("val") : 0));
	}
	return _res5;
}}).get())).doubleValue() / ((Number)sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	for (var x : g) {
		_res6.add(((Map)x).get("val"));
	}
	return _res6;
}}).get())).doubleValue());}});
	}
	return _res0;
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
