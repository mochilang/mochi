import java.util.*;
class DataClass1 {
	int id;
	String name;
	DataClass1(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class DataClass2 {
	int id;
	int customerId;
	DataClass2(int id, int customerId) {
		this.id = id;
		this.customerId = customerId;
	}
}
class DataClass3 {
	Object name;
	Object count;
	DataClass3(Object name, Object count) {
		this.name = name;
		this.count = count;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob"), new DataClass1(3, "Charlie")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1), new DataClass2(101, 1), new DataClass2(102, 2)));
	static List<DataClass3> stats = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res8 = new ArrayList<>();
	Map<Object,List<DataClass1>> _groups9 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp10 = new ArrayList<>();
		for (var _it11 : orders) {
			var o = _it11;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp10.add(_it11);
		}
		if (_tmp10.isEmpty()) _tmp10.add(null);
		for (var o : _tmp10) {
			Map<String,Object> _row12 = new HashMap<>();
			_row12.put("c", c);
			_row12.put("o", o);
			Object _key13 = c.name;
			List<DataClass1> _b14 = _groups9.get(_key13);
			if (_b14 == null) { _b14 = new ArrayList<>(); _groups9.put(_key13, _b14); }
			_b14.add(_row12);
		}
	}
	for (var __e : _groups9.entrySet()) {
		Object g_key = __e.getKey();
		List<DataClass1> g = __e.getValue();
		_res8.add(new DataClass3(g_key, count((new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res15 = new ArrayList<>();
	for (var r : g) {
		if (!(r.o)) continue;
		_res15.add(r);
	}
	return _res15;
}}).get())));
	}
	return _res8;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println("--- Group Left Join ---");
	for (Object s : stats) {
		System.out.println(((Map)s).get("name") + " " + "orders:" + " " + ((Map)s).get("count"));
	}
	}
}
