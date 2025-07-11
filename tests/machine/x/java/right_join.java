import java.util.*;

class IdName {
    int id;
    String name;
    IdName(int id, String name) {
        this.id = id;
        this.name = name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdName other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.name, other.name);
    }
    @Override public int hashCode() {
        return Objects.hash(id, name);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdCustomerIdTotal other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.customerId, other.customerId) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(id, customerId, total);
    }
    int size() { return 3; }
}
class CustomerNameOrder {
    String customerName;
    IdCustomerIdTotal order;
    CustomerNameOrder(String customerName, IdCustomerIdTotal order) {
        this.customerName = customerName;
        this.order = order;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CustomerNameOrder other)) return false;
        return Objects.equals(this.customerName, other.customerName) && Objects.equals(this.order, other.order);
    }
    @Override public int hashCode() {
        return Objects.hash(customerName, order);
    }
    int size() { return 2; }
}
public class RightJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob"), new IdName(3, "Charlie"), new IdName(4, "Diana")));
    List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 2, 125), new IdCustomerIdTotal(102, 1, 300)));
    List<CustomerNameOrder> result = (new java.util.function.Supplier<List<CustomerNameOrder>>(){public List<CustomerNameOrder> get(){
    List<CustomerNameOrder> _res0 = new ArrayList<>();
    for (var c : customers) {
        List<IdCustomerIdTotal> _tmp1 = new ArrayList<>();
        for (var _it2 : orders) {
            var o = _it2;
            if (!(Objects.equals(o.customerId, c.id))) continue;
            _tmp1.add(_it2);
        }
        if (_tmp1.isEmpty()) _tmp1.add(null);
        for (var o : _tmp1) {
            _res0.add(new CustomerNameOrder(c.name, o));
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Right Join using syntax ---");
    for (CustomerNameOrder entry : result) {
        if (entry.order != null) {
            System.out.println("Customer" + " " + entry.customerName + " " + "has order" + " " + entry.order.id + " " + "- $" + " " + entry.order.total);
        }
        else {
            System.out.println("Customer" + " " + entry.customerName + " " + "has no orders");
        }
    }
    }
}
