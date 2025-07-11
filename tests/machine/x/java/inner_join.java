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
class ResultCustomerNameOrderIdTotal {
	int orderId;
	String customerName;
	int total;
	ResultCustomerNameOrderIdTotal(int orderId, String customerName, int total) {
		this.orderId = orderId;
		this.customerName = customerName;
		this.total = total;
	}
}
public class Main {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob"), new CustomersIdName(3, "Charlie")));
	static List<OrdersCustomerIdIdTotal> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdIdTotal(100, 1, 250), new OrdersCustomerIdIdTotal(101, 2, 125), new OrdersCustomerIdIdTotal(102, 1, 300), new OrdersCustomerIdIdTotal(103, 4, 80)));
	static List<ResultCustomerNameOrderIdTotal> result = (new java.util.function.Supplier<List<ResultCustomerNameOrderIdTotal>>(){public List<ResultCustomerNameOrderIdTotal> get(){
	List<ResultCustomerNameOrderIdTotal> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_res1.add(new ResultCustomerNameOrderIdTotal(o.id, c.name, o.total));
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Orders with customer info ---");
	for (ResultCustomerNameOrderIdTotal entry : result) {
		System.out.println("Order" + " " + entry.orderId + " " + "by" + " " + entry.customerName + " " + "- $" + " " + entry.total);
	}
	}
}
