import java.util.*;
class IdName {
	int id;
	String name;
	IdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
	int size() { return 2; }
}
class IdCustomerId {
	int id;
	int customerId;
	IdCustomerId(int id, int customerId) {
		this.id = id;
		this.customerId = customerId;
	}
	int size() { return 2; }
}
class NameCount {
	Object name;
	int count;
	NameCount(Object name, int count) {
		this.name = name;
		this.count = count;
	}
	int size() { return 2; }
}
class OC {
	IdCustomerId o;
	IdName c;
	OC(IdCustomerId o, IdName c) {
		this.o = o;
		this.c = c;
	}
	int size() { return 2; }
}
public class GroupByJoin {
	public static void main(String[] args) {
	List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
	List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 1), new IdCustomerId(102, 2)));
	List<NameCount> stats = (new java.util.function.Supplier<List<NameCount>>(){public List<NameCount> get(){
	List<NameCount> _res0 = new ArrayList<>();
	Map<String,List<OC>> _groups1 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			OC _row2 = new OC(o, c);
			String _key3 = c.name;
			List<OC> _b4 = _groups1.get(_key3);
			if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
			_b4.add(_row2);
		}
	}
	for (Map.Entry<String,List<OC>> __e : _groups1.entrySet()) {
		String g_key = __e.getKey();
		List<OC> g = __e.getValue();
		_res0.add(new NameCount(g_key, g.size()));
	}
	return _res0;
}}).get();
	System.out.println("--- Orders per customer ---");
	for (NameCount s : stats) {
		System.out.println(s.name + " " + "orders:" + " " + s.count);
	}
	}
}
