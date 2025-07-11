import java.util.*;
class IdName {
	int id;
	String name;
	IdName(int id, String name) {
		this.id = id;
		this.name = name;
	}
	int size() { return 2; }
}
class IdCustomerId {
	int id;
	int customerId;
	IdCustomerId(int id, int customerId) {
		this.id = id;
		this.customerId = customerId;
	}
	int size() { return 2; }
}
class OrderIdSku {
	int orderId;
	String sku;
	OrderIdSku(int orderId, String sku) {
		this.orderId = orderId;
		this.sku = sku;
	}
	int size() { return 2; }
}
class OrderIdNameItem {
	int orderId;
	String name;
	OrderIdSku item;
	OrderIdNameItem(int orderId, String name, OrderIdSku item) {
		this.orderId = orderId;
		this.name = name;
		this.item = item;
	}
	int size() { return 3; }
}
public class LeftJoinMulti {
	public static void main(String[] args) {
	List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
	List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 2)));
	List<OrderIdSku> items = new ArrayList<>(Arrays.asList(new OrderIdSku(100, "a")));
	List<OrderIdNameItem> result = (new java.util.function.Supplier<List<OrderIdNameItem>>(){public List<OrderIdNameItem> get(){
	List<OrderIdNameItem> _res0 = new ArrayList<>();
	for (var o : orders) {
		for (var c : customers) {
			if (!(Objects.equals(o.customerId, c.id))) continue;
			List<OrderIdSku> _tmp1 = new ArrayList<>();
			for (var _it2 : items) {
				var i = _it2;
				if (!(Objects.equals(o.id, i.orderId))) continue;
				_tmp1.add(_it2);
			}
			if (_tmp1.isEmpty()) _tmp1.add(null);
			for (var i : _tmp1) {
				_res0.add(new OrderIdNameItem(o.id, c.name, i));
			}
		}
	}
	return _res0;
}}).get();
	System.out.println("--- Left Join Multi ---");
	for (OrderIdNameItem r : result) {
		System.out.println(r.orderId + " " + r.name + " " + r.item);
	}
	}
}
