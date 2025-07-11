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
class ItemsOrderIdSku {
	int orderId;
	String sku;
	ItemsOrderIdSku(int orderId, String sku) {
		this.orderId = orderId;
		this.sku = sku;
	}
}
class ResultNameSku {
	String name;
	String sku;
	ResultNameSku(String name, String sku) {
		this.name = name;
		this.sku = sku;
	}
}
public class JoinMulti {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob")));
	static List<OrdersCustomerIdId> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdId(100, 1), new OrdersCustomerIdId(101, 2)));
	static List<ItemsOrderIdSku> items = new ArrayList<>(Arrays.asList(new ItemsOrderIdSku(100, "a"), new ItemsOrderIdSku(101, "b")));
	static List<ResultNameSku> result = (new java.util.function.Supplier<List<ResultNameSku>>(){public List<ResultNameSku> get(){
	List<ResultNameSku> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			for (var i : items) {
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_res1.add(new ResultNameSku(c.name, i.sku));
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Multi Join ---");
	for (ResultNameSku r : result) {
		System.out.println(r.name + " " + "bought item" + " " + r.sku);
	}
	}
}
