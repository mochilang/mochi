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
class ResultCustomerOrderIdTotal {
	int orderId;
	CustomersIdName customer;
	int total;
	ResultCustomerOrderIdTotal(int orderId, CustomersIdName customer, int total) {
		this.orderId = orderId;
		this.customer = customer;
		this.total = total;
	}
}
public class Main {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob")));
	static List<OrdersCustomerIdIdTotal> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdIdTotal(100, 1, 250), new OrdersCustomerIdIdTotal(101, 3, 80)));
	static List<ResultCustomerOrderIdTotal> result = (new java.util.function.Supplier<List<ResultCustomerOrderIdTotal>>(){public List<ResultCustomerOrderIdTotal> get(){
	List<ResultCustomerOrderIdTotal> _res3 = new ArrayList<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(new ResultCustomerOrderIdTotal(o.id, c, o.total));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join ---");
	for (ResultCustomerOrderIdTotal entry : result) {
		System.out.println("Order" + " " + entry.orderId + " " + "customer" + " " + entry.customer + " " + "total" + " " + entry.total);
	}
	}
}
