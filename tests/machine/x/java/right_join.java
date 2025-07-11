import java.util.*;
class CustomersIdName {
	int id;
	String name;
	CustomersIdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class OrdersCustomerIdIdTotal {
	int id;
	int customerId;
	int total;
	OrdersCustomerIdIdTotal(int id, int customerId, int total) {
		this.id = id;
		this.customerId = customerId;
		this.total = total;
	}
}
class ResultCustomerNameOrder {
	String customerName;
	OrdersCustomerIdIdTotal order;
	ResultCustomerNameOrder(String customerName, OrdersCustomerIdIdTotal order) {
		this.customerName = customerName;
		this.order = order;
	}
}
public class RightJoin {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob"), new CustomersIdName(3, "Charlie"), new CustomersIdName(4, "Diana")));
	static List<OrdersCustomerIdIdTotal> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdIdTotal(100, 1, 250), new OrdersCustomerIdIdTotal(101, 2, 125), new OrdersCustomerIdIdTotal(102, 1, 300)));
	static List<ResultCustomerNameOrder> result = (new java.util.function.Supplier<List<ResultCustomerNameOrder>>(){public List<ResultCustomerNameOrder> get(){
	List<ResultCustomerNameOrder> _res3 = new ArrayList<>();
	for (var c : customers) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : orders) {
			var o = _it5;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var o : _tmp4) {
			_res3.add(new ResultCustomerNameOrder(c.name, o));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Right Join using syntax ---");
	for (ResultCustomerNameOrder entry : result) {
		if (entry.order) {
			System.out.println("Customer" + " " + entry.customerName + " " + "has order" + " " + entry.order.id + " " + "- $" + " " + entry.order.total);
		}
		else {
			System.out.println("Customer" + " " + entry.customerName + " " + "has no orders");
		}
	}
	}
}
