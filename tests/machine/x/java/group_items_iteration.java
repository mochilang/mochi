import java.util.*;
public class Main {
	static List<Map<Object,Object>> data = new ArrayList<>(java.util.Arrays.asList(new HashMap<>(java.util.Map.of("tag", "a", "val", 1)), new HashMap<>(java.util.Map.of("tag", "a", "val", 2)), new HashMap<>(java.util.Map.of("tag", "b", "val", 3))));
	static List<Object> groups = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Object>> _groups1 = new LinkedHashMap<>();
	for (var d : data) {
		var _row2 = d;
		Object _key3 = ((Map)d).get("tag");
		List<Object> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (var __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res0.add(g);
	}
	return _res0;
}}).get();
	static List<Object> tmp = new ArrayList<>(java.util.Arrays.asList());
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res5 = new ArrayList<>();
	for (var r : tmp) {
		_res5.add(r);
	}
	return _res5;
}}).get();
	static <T> List<T> append(List<T> list, T item) {
		List<T> res = new ArrayList<>(list);
		res.add(item);
		return res;
	}
	public static void main(String[] args) {
	for (var g : groups) {
		int total = 0;
		for (var x : ((Map)g).get("items")) {
			total = total + ((Map)x).get("val");
		}
		tmp = append(tmp, new HashMap<>(java.util.Map.of("tag", ((Map)g).get("key"), "total", total)));
	}
	System.out.println(result);
	}
}
