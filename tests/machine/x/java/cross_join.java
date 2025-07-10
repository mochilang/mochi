import java.util.*;
class DataClass1 {
	int id;
	String name;
	DataClass1(int id, String name) {
		this.id = id;
		this.name = name;
	}
}
class DataClass2 {
	int id;
	int customerId;
	int total;
	DataClass2(int id, int customerId, int total) {
		this.id = id;
		this.customerId = customerId;
		this.total = total;
	}
}
class DataClass3 {
	Object orderId;
	Object orderCustomerId;
	Object pairedCustomerName;
	Object orderTotal;
	DataClass3(Object orderId, Object orderCustomerId, Object pairedCustomerName, Object orderTotal) {
		this.orderId = orderId;
		this.orderCustomerId = orderCustomerId;
		this.pairedCustomerName = pairedCustomerName;
		this.orderTotal = orderTotal;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob"), new DataClass1(3, "Charlie")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1, 250), new DataClass2(101, 2, 125), new DataClass2(102, 1, 300)));
	static List<DataClass3> result = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			_res1.add(new DataClass3(o.id, o.customerId, c.name, o.total));
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Cross Join: All order-customer pairs ---");
	for (Object entry : result) {
		System.out.println("Order" + " " + ((Map)entry).get("orderId") + " " + "(customerId:" + " " + ((Map)entry).get("orderCustomerId") + " " + ", total: $" + " " + ((Map)entry).get("orderTotal") + " " + ") paired with" + " " + ((Map)entry).get("pairedCustomerName"));
	}
	}
}
