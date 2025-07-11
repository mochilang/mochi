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
class ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName {
	int orderId;
	int orderCustomerId;
	String pairedCustomerName;
	int orderTotal;
	ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName(int orderId, int orderCustomerId, String pairedCustomerName, int orderTotal) {
		this.orderId = orderId;
		this.orderCustomerId = orderCustomerId;
		this.pairedCustomerName = pairedCustomerName;
		this.orderTotal = orderTotal;
	}
}
public class Main {
	static List<CustomersIdName> customers = new ArrayList<>(Arrays.asList(new CustomersIdName(1, "Alice"), new CustomersIdName(2, "Bob"), new CustomersIdName(3, "Charlie")));
	static List<OrdersCustomerIdIdTotal> orders = new ArrayList<>(Arrays.asList(new OrdersCustomerIdIdTotal(100, 1, 250), new OrdersCustomerIdIdTotal(101, 2, 125), new OrdersCustomerIdIdTotal(102, 1, 300)));
	static List<ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName> result = (new java.util.function.Supplier<List<ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName>>(){public List<ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName> get(){
	List<ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res1.add(new ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName(o.id, o.customerId, c.name, o.total));
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (ResultOrderCustomerIdOrderIdOrderTotalPairedCustomerName entry : result) {
		System.out.println("Order" + " " + entry.orderId + " " + "(customerId:" + " " + entry.orderCustomerId + " " + ", total: $" + " " + entry.orderTotal + " " + ") paired with" + " " + entry.pairedCustomerName);
	}
	}
}
