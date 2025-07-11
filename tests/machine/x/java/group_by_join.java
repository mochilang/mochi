import java.util.*;
class OC {
	Map<String,Integer> o;
	Map<String,Object> c;
	OC(Map<String,Integer> o, Map<String,Object> c) {
		this.o = o;
		this.c = c;
	}
	int size() { return 2; }
}
public class GroupByJoin {
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	List<Map<String,Object>> customers = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob"))));
	List<Map<String,Integer>> orders = new ArrayList<>(Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 1)), mapOfEntries(entry("id", 102), entry("customerId", 2))));
	List<Map<String,Object>> stats = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res0 = new ArrayList<>();
	Map<Object,List<OC>> _groups1 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Map)o).get("customerId"), ((Map)c).get("id")))) continue;
			OC _row2 = new OC(o, c);
			Object _key3 = ((Map)c).get("name");
			List<OC> _b4 = _groups1.get(_key3);
			if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
			_b4.add(_row2);
		}
	}
	for (Map.Entry<Object,List<OC>> __e : _groups1.entrySet()) {
		Object g_key = __e.getKey();
		List<OC> g = __e.getValue();
		_res0.add(mapOfEntries(entry("name", g_key), entry("count", g.size())));
	}
	return _res0;
}}).get();
	System.out.println("--- Orders per customer ---");
	for (Map<String,Object> s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
