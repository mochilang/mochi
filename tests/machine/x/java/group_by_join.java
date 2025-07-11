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
class OC {
	OrdersCustomerIdId o;
	CustomersIdName c;
	OC(OrdersCustomerIdId o, CustomersIdName c) {
		this.o = o;
		this.c = c;
	}
}
public class Main {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob")));
	static List<OrdersCustomerIdId> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdId(100, 1), new OrdersCustomerIdId(101, 1), new OrdersCustomerIdId(102, 2)));
	static List<StatsCountName> stats = (new java.util.function.Supplier<List<StatsCountName>>(){public List<StatsCountName> get(){
	List<StatsCountName> _res5 = new ArrayList<>();
	Map<String,List<OC>> _groups6 = new LinkedHashMap<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			OC _row7 = new OC(o, c);
			String _key8 = c.name;
			List<OC> _b9 = _groups6.get(_key8);
			if (_b9 == null) { _b9 = new ArrayList<>(); _groups6.put(_key8, _b9); }
			_b9.add(_row7);
		}
	}
	for (var __e : _groups6.entrySet()) {
		String g_key = __e.getKey();
		List<OC> g = __e.getValue();
		_res5.add(new StatsCountName(g_key, count(g)));
	}
	return _res5;
}}).get();
	static int count(Collection<?> c) {
		return c.size();
	}
	public static void main(String[] args) {
	System.out.println("--- Orders per customer ---");
	for (StatsCountName s : stats) {
		System.out.println(s.name + " " + "orders:" + " " + s.count);
	}
	}
}
