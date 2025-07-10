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
	int orderId;
	DataClass1 customer;
	int total;
	DataClass3(int orderId, DataClass1 customer, int total) {
		this.orderId = orderId;
		this.customer = customer;
		this.total = total;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1, 250), new DataClass2(101, 3, 80)));
	static List<DataClass3> result = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res3 = new ArrayList<>();
	for (var o : orders) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : customers) {
			var c = _it5;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var c : _tmp4) {
			_res3.add(new DataClass3(o.id, c, o.total));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join ---");
	for (DataClass3 entry : result) {
		System.out.println("Order" + " " + entry.orderId + " " + "customer" + " " + entry.customer + " " + "total" + " " + entry.total);
	}
	}
}
