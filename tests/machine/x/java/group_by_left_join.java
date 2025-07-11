import java.util.*;
class CustomersIdName {
	int id;
	String name;
	CustomersIdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class OrdersCustomerIdId {
	int id;
	int customerId;
	OrdersCustomerIdId(int id, int customerId) {
		this.id = id;
		this.customerId = customerId;
	}
}
class StatsCountName {
	Object name;
	int count;
	StatsCountName(Object name, int count) {
		this.name = name;
		this.count = count;
	}
}
class CO {
	CustomersIdName c;
	OrdersCustomerIdId o;
	CO(CustomersIdName c, OrdersCustomerIdId o) {
		this.c = c;
		this.o = o;
	}
}
public class Main {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob"), new CustomersIdName(3, "Charlie")));
	static List<OrdersCustomerIdId> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdId(100, 1), new OrdersCustomerIdId(101, 1), new OrdersCustomerIdId(102, 2)));
	static List<StatsCountName> stats = (new java.util.function.Supplier<List<StatsCountName>>(){public List<StatsCountName> get(){
	List<StatsCountName> _res8 = new ArrayList<>();
	Map<String,List<CO>> _groups9 = new LinkedHashMap<>();
	for (var c : customers) {
		List<Object> _tmp10 = new ArrayList<>();
		for (var _it11 : orders) {
			var o = _it11;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp10.add(_it11);
		}
		if (_tmp10.isEmpty()) _tmp10.add(null);
		for (var o : _tmp10) {
			CO _row12 = new CO(c, o);
			String _key13 = c.name;
			List<CO> _b14 = _groups9.get(_key13);
			if (_b14 == null) { _b14 = new ArrayList<>(); _groups9.put(_key13, _b14); }
			_b14.add(_row12);
		}
	}
	for (var __e : _groups9.entrySet()) {
		String g_key = __e.getKey();
		List<CO> g = __e.getValue();
		_res8.add(new StatsCountName(g_key, (new java.util.function.Supplier<List<CO>>(){public List<CO> get(){
	List<CO> _res15 = new ArrayList<>();
	for (var r : g) {
		if (!(r.o)) continue;
		_res15.add(r);
	}
	return _res15;
}}).get().size()));
	}
	return _res8;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Group Left Join ---");
	for (StatsCountName s : stats) {
		System.out.println(s.name + " " + "orders:" + " " + s.count);
	}
	}
}
