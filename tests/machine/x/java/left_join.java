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
}
class OrderIdCustomerTotal {
    int orderId;
    IdName customer;
    int total;
    OrderIdCustomerTotal(int orderId, IdName customer, int total) {
        this.orderId = orderId;
        this.customer = customer;
        this.total = total;
    }
    int size() { return 3; }
}
public class LeftJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
    List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 3, 80)));
    List<OrderIdCustomerTotal> result = (new java.util.function.Supplier<List<OrderIdCustomerTotal>>(){public List<OrderIdCustomerTotal> get(){
    List<OrderIdCustomerTotal> _res0 = new ArrayList<>();
    for (var o : orders) {
        List<IdName> _tmp1 = new ArrayList<>();
        for (var _it2 : customers) {
            var c = _it2;
            if (!(Objects.equals(o.customerId, c.id))) continue;
            _tmp1.add(_it2);
        }
        if (_tmp1.isEmpty()) _tmp1.add(null);
        for (var c : _tmp1) {
            _res0.add(new OrderIdCustomerTotal(o.id, c, o.total));
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Left Join ---");
    for (OrderIdCustomerTotal entry : result) {
        System.out.println("Order" + " " + entry.orderId + " " + "customer" + " " + entry.customer + " " + "total" + " " + entry.total);
    }
    }
}
