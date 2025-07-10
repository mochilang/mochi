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
	DataClass2 o;
	DataClass1 c;
	DataClass4(DataClass2 o, DataClass1 c) {
		this.o = o;
		this.c = c;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1), new DataClass2(101, 1), new DataClass2(102, 2)));
	static List<DataClass3> stats = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res5 = new ArrayList<>();
	Map<String,List<DataClass4>> _groups6 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			DataClass4 _row7 = new DataClass4(o, c);
			String _key8 = c.name;
			List<DataClass4> _b9 = _groups6.get(_key8);
			if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
			_b9.add(_row7);
		}
	}
	for (var __e : _groups6.entrySet()) {
		String g_key = __e.getKey();
		List<DataClass4> g = __e.getValue();
		_res5.add(new DataClass3(g_key, count(g)));
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println("--- Orders per customer ---");
	for (DataClass3 s : stats) {
		System.out.println(s.name + " " + "orders:" + " " + s.count);
	}
	}
}
