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
	DataClass2(int id, int customerId) {
		this.id = id;
		this.customerId = customerId;
	}
}
class DataClass3 {
	int orderId;
	String sku;
	DataClass3(int orderId, String sku) {
		this.orderId = orderId;
		this.sku = sku;
	}
}
class DataClass4 {
	Object orderId;
	Object name;
	Object item;
	DataClass4(Object orderId, Object name, Object item) {
		this.orderId = orderId;
		this.name = name;
		this.item = item;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1), new DataClass2(101, 2)));
	static List<DataClass3> items = new ArrayList<>(java.util.Arrays.asList(new DataClass3(100, "a")));
	static List<DataClass4> result = (new java.util.function.Supplier<List<DataClass4>>(){public List<DataClass4> get(){
	List<DataClass4> _res3 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			List<Object> _tmp4 = new ArrayList<>();
			for (var _it5 : items) {
				var i = _it5;
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_tmp4.add(_it5);
			}
			if (_tmp4.isEmpty()) _tmp4.add(null);
			for (var i : _tmp4) {
				_res3.add(new DataClass4(o.id, c.name, i));
			}
		}
	}
	return _res3;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Left Join Multi ---");
	for (Object r : result) {
		System.out.println(((Map)r).get("orderId") + " " + ((Map)r).get("name") + " " + ((Map)r).get("item"));
	}
	}
}
