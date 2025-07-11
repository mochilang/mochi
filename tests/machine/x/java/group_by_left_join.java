import java.util.*;
class CO {
	Map<String,Object> c;
	Map<String,Integer> o;
	CO(Map<String,Object> c, Map<String,Integer> o) {
		this.c = c;
		this.o = o;
	}
	int size() { return 2; }
}
public class GroupByLeftJoin {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob")), mapOfEntries(entry("id", 3), entry("name", "Charlie"))));
	List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 1)), mapOfEntries(entry("id", 102), entry("customerId", 2))));
	List<Map<String,Object>> stats = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<Object,List<CO>> _groups1 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Map<String,Integer>> _tmp2 = new ArrayList<>();
		for (var _it3 : orders) {
			var o = _it3;
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			_tmp2.add(_it3);
		}
		if (_tmp2.isEmpty()) _tmp2.add(null);
		for (var o : _tmp2) {
			CO _row4 = new CO(c, o);
			Object _key5 = ((Map)c).get("name");
			List<CO> _b6 = _groups1.get(_key5);
			if (_b6 == null) { _b6 = new ArrayList<>(); _groups1.put(_key5, _b6); }
			_b6.add(_row4);
		}
	}
	for (Map.Entry<Object,List<CO>> __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<CO> g = __e.getValue();
		_res0.add(mapOfEntries(entry("name", g_key), entry("count", (new java.util.function.Supplier<List<CO>>(){public List<CO> get(){
	List<CO> _res7 = new ArrayList<>();
	for (var r : g) {
		if (!(r.o != null)) continue;
		_res7.add(r);
	}
	return _res7;
}}).get().size())));
	}
	return _res0;
}}).get();
	System.out.println("--- Group Left Join ---");
	for (Map<String,Object> s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
