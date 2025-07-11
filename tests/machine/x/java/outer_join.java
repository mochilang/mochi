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
class ResultCustomerOrder {
	OrdersCustomerIdIdTotal order;
	CustomersIdName customer;
	ResultCustomerOrder(OrdersCustomerIdIdTotal order, CustomersIdName customer) {
		this.order = order;
		this.customer = customer;
	}
}
public class OuterJoin {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob"), new CustomersIdName(3, "Charlie"), new CustomersIdName(4, "Diana")));
	static List<OrdersCustomerIdIdTotal> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdIdTotal(100, 1, 250), new OrdersCustomerIdIdTotal(101, 2, 125), new OrdersCustomerIdIdTotal(102, 1, 300), new OrdersCustomerIdIdTotal(103, 5, 80)));
	static List<ResultCustomerOrder> result = (new java.util.function.Supplier<List<ResultCustomerOrder>>(){public List<ResultCustomerOrder> get(){
	List<ResultCustomerOrder> _res3 = new ArrayList<>();
	java.util.Set<Object> _matched = new java.util.HashSet<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp4.add(_it5);
			_matched.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(new ResultCustomerOrder(o, c));
		}
	}
	for (var c : customers) {
		if (!_matched.contains(c)) {
			Object o = null;
			_res3.add(new ResultCustomerOrder(o, c));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Outer Join using syntax ---");
	for (ResultCustomerOrder row : result) {
		if (row.order) {
			if (row.customer) {
				System.out.println("Order" + " " + row.order.id + " " + "by" + " " + row.customer.name + " " + "- $" + " " + row.order.total);
			}
			else {
				System.out.println("Order" + " " + row.order.id + " " + "by" + " " + "Unknown" + " " + "- $" + " " + row.order.total);
			}
		}
		else {
			System.out.println("Customer" + " " + row.customer.name + " " + "has no orders");
		}
	}
	}
}
