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
	DataClass2 order;
	DataClass1 customer;
	DataClass3(DataClass2 order, DataClass1 customer) {
		this.order = order;
		this.customer = customer;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob"), new DataClass1(3, "Charlie"), new DataClass1(4, "Diana")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1, 250), new DataClass2(101, 2, 125), new DataClass2(102, 1, 300), new DataClass2(103, 5, 80)));
	static List<DataClass3> result = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res3 = new ArrayList<>();
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
			_res3.add(new DataClass3(o, c));
		}
	}
	for (var c : customers) {
		if (!_matched.contains(c)) {
			Object o = null;
			_res3.add(new DataClass3(o, c));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Outer Join using syntax ---");
	for (DataClass3 row : result) {
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
