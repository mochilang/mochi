import java.util.*;
class OC {
	Map<String,Integer> o;
	Map<String,Object> c;
	OC(Map<String,Integer> o, Map<String,Object> c) {
		this.o = o;
		this.c = c;
	}
}
public class Main {
	static List<Map<String,Object>> customers = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 1), entry("name", "Alice")), mapOfEntries(entry("id", 2), entry("name", "Bob"))));
	static List<Map<String,Integer>> orders = new ArrayList<>(java.util.Arrays.asList(mapOfEntries(entry("id", 100), entry("customerId", 1)), mapOfEntries(entry("id", 101), entry("customerId", 1)), mapOfEntries(entry("id", 102), entry("customerId", 2))));
	static List<Map<String,Object>> stats = (new java.util.function.Supplier<List<Map<String,Object>>>(){public List<Map<String,Object>> get(){
	List<Map<String,Object>> _res5 = new ArrayList<>();
	Map<Object,List<OC>> _groups6 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(((Integer)((Map)o).get("customerId")), ((Map)c).get("id")))) continue;
			OC _row7 = new OC(o, c);
			Object _key8 = ((Map)c).get("name");
			List<OC> _b9 = _groups6.get(_key8);
			if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
			_b9.add(_row7);
		}
	}
	for (var __e : _groups6.entrySet()) {
		Object g_key = __e.getKey();
		List<OC> g = __e.getValue();
		_res5.add(mapOfEntries(entry("name", g_key), entry("count", count(g))));
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println("--- Orders per customer ---");
	for (Map<String,Object> s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
