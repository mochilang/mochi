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
class OrderCustomer {
    IdCustomerIdTotal order;
    IdName customer;
    OrderCustomer(IdCustomerIdTotal order, IdName customer) {
        this.order = order;
        this.customer = customer;
    }
    int size() { return 2; }
}
public class OuterJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob"), new IdName(3, "Charlie"), new IdName(4, "Diana")));
    List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 2, 125), new IdCustomerIdTotal(102, 1, 300), new IdCustomerIdTotal(103, 5, 80)));
    List<OrderCustomer> result = (new java.util.function.Supplier<List<OrderCustomer>>(){public List<OrderCustomer> get(){
    List<OrderCustomer> _res0 = new ArrayList<>();
    java.util.Set<Object> _matched = new java.util.HashSet<>();
    for (var o : orders) {
        List<IdName> _tmp1 = new ArrayList<>();
        for (var _it2 : customers) {
            var c = _it2;
            if (!(Objects.equals(o.customerId, c.id))) continue;
            _tmp1.add(_it2);
            _matched.add(_it2);
        }
        if (_tmp1.isEmpty()) _tmp1.add(null);
        for (var c : _tmp1) {
            _res0.add(new OrderCustomer(o, c));
        }
    }
    for (var c : customers) {
        if (!_matched.contains(c)) {
            Object o = null;
            _res0.add(new OrderCustomer(o, c));
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Outer Join using syntax ---");
    for (OrderCustomer row : result) {
        if (row.order != null) {
            if (row.customer != null) {
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
