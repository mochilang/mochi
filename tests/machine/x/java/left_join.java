// left_join.mochi
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
class OrderIdCustomerTotal {
    int orderId;
    IdName customer;
    int total;
    OrderIdCustomerTotal(int orderId, IdName customer, int total) {
        this.orderId = orderId;
        this.customer = customer;
        this.total = total;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderIdCustomerTotal other)) return false;
        return Objects.equals(this.orderId, other.orderId) && Objects.equals(this.customer, other.customer) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(orderId, customer, total);
    }
    int size() { return 3; }
}
public class LeftJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
    List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 3, 80)));
    List<OrderIdCustomerTotal> result = (new java.util.function.Supplier<List<OrderIdCustomerTotal>>(){public List<OrderIdCustomerTotal> get(){
    List<OrderIdCustomerTotal> res0 = new ArrayList<>();
    for (var o : orders) {
        List<IdName> tmp1 = new ArrayList<>();
        for (var it2 : customers) {
            var c = it2;
            if (!(Objects.equals(o.customerId, c.id))) continue;
            tmp1.add(it2);
        }
        if (tmp1.isEmpty()) tmp1.add(null);
        for (var c : tmp1) {
            res0.add(new OrderIdCustomerTotal(o.id, c, o.total));
        }
    }
    return res0;
}}).get();
    System.out.println("--- Left Join ---");
    for (OrderIdCustomerTotal entry : result) {
        System.out.println("Order" + " " + entry.orderId + " " + "customer" + " " + entry.customer + " " + "total" + " " + entry.total);
    }
    }
}
