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
class ResultItemNameOrderId {
	int orderId;
	String name;
	ItemsOrderIdSku item;
	ResultItemNameOrderId(int orderId, String name, ItemsOrderIdSku item) {
		this.orderId = orderId;
		this.name = name;
		this.item = item;
	}
}
public class LeftJoinMulti {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob")));
	static List<OrdersCustomerIdId> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdId(100, 1), new OrdersCustomerIdId(101, 2)));
	static List<ItemsOrderIdSku> items = new ArrayList<>(Arrays.asList(new ItemsOrderIdSku(100, "a")));
	static List<ResultItemNameOrderId> result = (new java.util.function.Supplier<List<ResultItemNameOrderId>>(){public List<ResultItemNameOrderId> get(){
	List<ResultItemNameOrderId> _res3 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			List<Object> _tmp4 = new ArrayList<>();
			for (var _it5 : items) {
				var i = _it5;
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_tmp4.add(_it5);
			}
			if (_tmp4.isEmpty()) _tmp4.add(null);
			for (var i : _tmp4) {
				_res3.add(new ResultItemNameOrderId(o.id, c.name, i));
			}
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join Multi ---");
	for (ResultItemNameOrderId r : result) {
		System.out.println(r.orderId + " " + r.name + " " + r.item);
	}
	}
}
