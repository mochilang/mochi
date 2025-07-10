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
	String name;
	String sku;
	DataClass4(String name, String sku) {
		this.name = name;
		this.sku = sku;
	}
}
public class Main {
	static List<DataClass1> customers = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "Alice"), new DataClass1(2, "Bob")));
	static List<DataClass2> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass2(100, 1), new DataClass2(101, 2)));
	static List<DataClass3> items = new ArrayList<>(java.util.Arrays.asList(new DataClass3(100, "a"), new DataClass3(101, "b")));
	static List<DataClass4> result = (new java.util.function.Supplier<List<DataClass4>>(){public List<DataClass4> get(){
	List<DataClass4> _res1 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			for (var i : items) {
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_res1.add(new DataClass4(c.name, i.sku));
			}
		}
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Multi Join ---");
	for (DataClass4 r : result) {
		System.out.println(r.name + " " + "bought item" + " " + r.sku);
	}
	}
}
