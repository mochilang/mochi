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
	Object customerName;
	Object order;
	DataClass3(Object customerName, Object order) {
		this.customerName = customerName;
		this.order = order;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob"), new DataClass1(3, "Charlie"), new DataClass1(4, "Diana")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1, 250), new DataClass2(101, 2, 125), new DataClass2(102, 1, 300)));
	static List<DataClass3> result = (new java.util.function.Supplier<List<DataClass3>>(){public List<DataClass3> get(){
	List<DataClass3> _res3 = new ArrayList<>();
	for (var c : customers) {
		List<Object> _tmp4 = new ArrayList<>();
		for (var _it5 : orders) {
			var o = _it5;
			if (!(Objects.equals(o.customerId, c.id))) continue;
			_tmp4.add(_it5);
		}
		if (_tmp4.isEmpty()) _tmp4.add(null);
		for (var o : _tmp4) {
			_res3.add(new DataClass3(c.name, o));
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Right Join using syntax ---");
	for (Object entry : result) {
		if (((Map)entry).get("order") != null) {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has order" + " " + ((Map)((Map)entry).get("order")).get("id") + " " + "- $" + " " + ((Map)((Map)entry).get("order")).get("total"));
		}
		else {
			System.out.println("Customer" + " " + ((Map)entry).get("customerName") + " " + "has no orders");
		}
	}
	}
}
