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
class OrderIdSku {
	int orderId;
	String sku;
	OrderIdSku(int orderId, String sku) {
		this.orderId = orderId;
		this.sku = sku;
	}
	int size() { return 2; }
}
class NameSku {
	String name;
	String sku;
	NameSku(String name, String sku) {
		this.name = name;
		this.sku = sku;
	}
	int size() { return 2; }
}
public class JoinMulti {
	public static void main(String[] args) {
	List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
	List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 2)));
	List<OrderIdSku> items = new ArrayList<>(Arrays.asList(new OrderIdSku(100, "a"), new OrderIdSku(101, "b")));
	List<NameSku> result = (new java.util.function.Supplier<List<NameSku>>(){public List<NameSku> get(){
	List<NameSku> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			for (var i : items) {
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_res0.add(new NameSku(c.name, i.sku));
			}
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Multi Join ---");
	for (NameSku r : result) {
		System.out.println(r.name + " " + "bought item" + " " + r.sku);
	}
	}
}
