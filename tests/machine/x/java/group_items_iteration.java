import java.util.*;
public class GroupItemsIteration {
	static <T> List<T> append(List<T> list, T item) {
		List<T> res = new ArrayList<>(list);
		res.add(item);
		return res;
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> data = new ArrayList<>(Arrays.asList(mapOfEntries(entry("tag", "a"), entry("val", 1)), mapOfEntries(entry("tag", "a"), entry("val", 2)), mapOfEntries(entry("tag", "b"), entry("val", 3))));
	List<Object> groups = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res0 = new ArrayList<>();
	Map<Object,List<Map<String,Object>>> _groups1 = new LinkedHashMap<>();
	for (var d : data) {
		var _row2 = d;
		Object _key3 = ((Map)d).get("tag");
		List<Map<String,Object>> _b4 = _groups1.get(_key3);
		if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
		_b4.add(_row2);
	}
	for (Map.Entry<Object,List<Map<String,Object>>> __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<Map<String,Object>> g = __e.getValue();
		_res0.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("key", g_key), Map.entry("items", g))));
	}
	return _res0;
}}).get();
	List<Object> tmp = new ArrayList<>(Arrays.asList());
	for (Object g : groups) {
		int total = 0;
		for (Object x : (List)((Map)g).get("items")) {
			total = (int)(total + ((Number)((Map)x).get("val")).doubleValue());
		}
		tmp = append(tmp, mapOfEntries(entry("tag", ((Map)g).get("key")), entry("total", total)));
	}
	List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res5 = new ArrayList<>();
	for (var r : tmp) {
		_res5.add(r);
	}
	return _res5;
}}).get();
	System.out.println(result);
	}
}
