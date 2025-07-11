import java.util.*;
class IdName {
	int id;
	String name;
	IdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof IdName other)) return false;
		return Objects.equals(this.id, other.id) && Objects.equals(this.name, other.name);
	}
	@Override public int hashCode() {
		return Objects.hash(id, name);
	}
}
class IdCustomerIdTotal {
	int id;
	int customerId;
	int total;
	IdCustomerIdTotal(int id, int customerId, int total) {
		this.id = id;
		this.customerId = customerId;
		this.total = total;
	}
	int size() { return 3; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof IdCustomerIdTotal other)) return false;
		return Objects.equals(this.id, other.id) && Objects.equals(this.customerId, other.customerId) && Objects.equals(this.total, other.total);
	}
	@Override public int hashCode() {
		return Objects.hash(id, customerId, total);
	}
}
class OrderIdOrderCustomerIdPairedCustomerNameOrderTotal {
	int orderId;
	int orderCustomerId;
	String pairedCustomerName;
	int orderTotal;
	OrderIdOrderCustomerIdPairedCustomerNameOrderTotal(int orderId, int orderCustomerId, String pairedCustomerName, int orderTotal) {
		this.orderId = orderId;
		this.orderCustomerId = orderCustomerId;
		this.pairedCustomerName = pairedCustomerName;
		this.orderTotal = orderTotal;
	}
	int size() { return 4; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof OrderIdOrderCustomerIdPairedCustomerNameOrderTotal other)) return false;
		return Objects.equals(this.orderId, other.orderId) && Objects.equals(this.orderCustomerId, other.orderCustomerId) && Objects.equals(this.pairedCustomerName, other.pairedCustomerName) && Objects.equals(this.orderTotal, other.orderTotal);
	}
	@Override public int hashCode() {
		return Objects.hash(orderId, orderCustomerId, pairedCustomerName, orderTotal);
	}
}
public class CrossJoin {
	public static void main(String[] args) {
	List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob"), new IdName(3, "Charlie")));
	List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 2, 125), new IdCustomerIdTotal(102, 1, 300)));
	List<OrderIdOrderCustomerIdPairedCustomerNameOrderTotal> result = (new java.util.function.Supplier<List<OrderIdOrderCustomerIdPairedCustomerNameOrderTotal>>(){public List<OrderIdOrderCustomerIdPairedCustomerNameOrderTotal> get(){
	List<OrderIdOrderCustomerIdPairedCustomerNameOrderTotal> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res0.add(new OrderIdOrderCustomerIdPairedCustomerNameOrderTotal(o.id, o.customerId, c.name, o.total));
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (OrderIdOrderCustomerIdPairedCustomerNameOrderTotal entry : result) {
		System.out.println("Order" + " " + entry.orderId + " " + "(customerId:" + " " + entry.orderCustomerId + " " + ", total: $" + " " + entry.orderTotal + " " + ") paired with" + " " + entry.pairedCustomerName);
	}
	}
}
