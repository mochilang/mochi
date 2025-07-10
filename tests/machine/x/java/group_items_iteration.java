import java.util.*;
class DataClass1 {
	String tag;
	int val;
	DataClass1(String tag, int val) {
		this.tag = tag;
		this.val = val;
	}
}
class DataClass2 {
	Object tag;
	int total;
	DataClass2(Object tag, int total) {
		this.tag = tag;
		this.total = total;
	}
}
public class Main {
	static List<DataClass1> data = new ArrayList<>(java.util.Arrays.asList(new DataClass1("a", 1), new DataClass1("a", 2), new DataClass1("b", 3)));
	static List<Object> groups = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res6 = new ArrayList<>();
	Map<Object,List<DataClass1>> _groups7 = new LinkedHashMap<>();
	for (var d : data) {
		var _row8 = d;
		Object _key9 = d.tag;
		List<DataClass1> _b10 = _groups7.get(_key9);
		if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
		_b10.add(_row8);
	}
	for (var __e : _groups7.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		_res6.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("key", g_key), Map.entry("items", g))));
	}
	return _res6;
}}).get();
	static List<Object> tmp = new ArrayList<>(java.util.Arrays.asList());
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
		tmp = append(tmp, new DataClass2(((Map)g).get("key"), total));
	}
	System.out.println(result);
	}
}
