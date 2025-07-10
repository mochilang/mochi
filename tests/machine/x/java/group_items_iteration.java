import java.util.*;
public class Main {
	static List<Map<String,Object>> data = new ArrayList<>(java.util.Arrays.asList(Main.<String,Object>mapOf("tag", "a", "val", 1), Main.<String,Object>mapOf("tag", "a", "val", 2), Main.<String,Object>mapOf("tag", "b", "val", 3)));
	static List<Object> groups = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res6 = new ArrayList<>();
	Map<Object,List<Object>> _groups7 = new LinkedHashMap<>();
	for (var d : data) {
		var _row8 = d;
		Object _key9 = ((Map)d).get("tag");
		List<Object> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<Object> g = __e.getValue();
		_res6.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("key", g_key), Map.entry("items", g))));
	}
	return _res6;
}}).get();
	static List<Object> tmp = new ArrayList<>(java.util.Arrays.asList());
	static List<Object> result = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res11 = new ArrayList<>();
	for (var r : tmp) {
		_res11.add(r);
	}
	return _res11;
}}).get();
	static <T> List<T> append(List<T> list, T item) {
		List<T> res = new ArrayList<>(list);
		res.add(item);
		return res;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	for (var g : groups) {
		int total = 0;
		for (var x : (List)((Map)g).get("items")) {
			total = (int)(total + ((Number)((Map)x).get("val")).doubleValue());
		}
		tmp = append(tmp, Main.<String,Object>mapOf("tag", ((Map)g).get("key"), "total", total));
	}
	System.out.println(result);
	}
}
