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
class DataClass4 {
	DataClass1 c;
	DataClass2 o;
	DataClass4(DataClass1 c, DataClass2 o) {
		this.c = c;
		this.o = o;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob"), new DataClass1(3, "Charlie")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1), new DataClass2(101, 1), new DataClass2(102, 2)));
	static List<DataClass3> stats = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res8 = new ArrayList<>();
	Map<String,List<DataClass4>> _groups9 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp10 = new ArrayList<>();
		for (var _it11 : orders) {
			var o = _it11;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp10.add(_it11);
		}
		if (_tmp10.isEmpty()) _tmp10.add(null);
		for (var o : _tmp10) {
			DataClass4 _row12 = new DataClass4(c, o);
			String _key13 = c.name;
			List<DataClass4> _b14 = _groups9.get(_key13);
			if (_b14 == null) { _b14 = new ArrayList<>(); _groups9.put(_key13, _b14); }
			_b14.add(_row12);
		}
	}
	for (var __e : _groups9.entrySet()) {
		String g_key = __e.getKey();
		List<DataClass4> g = __e.getValue();
		_res8.add(new DataClass3(g_key, count((new java.util.function.Supplier<List<DataClass4>>(){public List<DataClass4> get(){
	List<DataClass4> _res15 = new ArrayList<>();
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
	for (DataClass3 s : stats) {
		System.out.println(s.name + " " + "orders:" + " " + s.count);
	}
	}
}
