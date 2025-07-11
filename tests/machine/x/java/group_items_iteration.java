import java.util.*;
class DataTagVal {
	String tag;
	int val;
	DataTagVal(String tag, int val) {
		this.tag = tag;
		this.val = val;
	}
}
class TmpTagTotal {
	Object tag;
	int total;
	TmpTagTotal(Object tag, int total) {
		this.tag = tag;
		this.total = total;
	}
}
public class Main {
	static List<DataTagVal> data = new ArrayList<>(Arrays.asList(new DataTagVal("a", 1), new DataTagVal("a", 2), new DataTagVal("b", 3)));
	static List<Object> groups = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res6 = new ArrayList<>();
	Map<String,List<DataTagVal>> _groups7 = new LinkedHashMap<>();
	for (var d : data) {
		var _row8 = d;
		String _key9 = d.tag;
		List<DataTagVal> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		String g_key = __e.getKey();
		List<DataTagVal> g = __e.getValue();
		_res6.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("key", g_key), Map.entry("items", g))));
	}
	return _res6;
}}).get();
	static List<Object> tmp = new ArrayList<>(Arrays.asList());
	static List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
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
	public static void main(String[] args) {
	for (Object g : groups) {
		int total = 0;
		for (Object x : (List)((Map)g).get("items")) {
			total = (int)(total + ((Number)((Map)x).get("val")).doubleValue());
		}
		tmp = append(tmp, new TmpTagTotal(((Map)g).get("key"), total));
	}
	System.out.println(result);
	}
}
