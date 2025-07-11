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
class OrderIdCustomerNameTotal {
    int orderId;
    String customerName;
    int total;
    OrderIdCustomerNameTotal(int orderId, String customerName, int total) {
        this.orderId = orderId;
        this.customerName = customerName;
        this.total = total;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderIdCustomerNameTotal other)) return false;
        return Objects.equals(this.orderId, other.orderId) && Objects.equals(this.customerName, other.customerName) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(orderId, customerName, total);
    }
    int size() { return 3; }
}
public class InnerJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob"), new IdName(3, "Charlie")));
    List<IdCustomerIdTotal> orders = new ArrayList<>(Arrays.asList(new IdCustomerIdTotal(100, 1, 250), new IdCustomerIdTotal(101, 2, 125), new IdCustomerIdTotal(102, 1, 300), new IdCustomerIdTotal(103, 4, 80)));
    List<OrderIdCustomerNameTotal> result = (new java.util.function.Supplier<List<OrderIdCustomerNameTotal>>(){public List<OrderIdCustomerNameTotal> get(){
    List<OrderIdCustomerNameTotal> _res0 = new ArrayList<>();
    for (var o : orders) {
        for (var c : customers) {
            if (!(Objects.equals(o.customerId, c.id))) continue;
            _res0.add(new OrderIdCustomerNameTotal(o.id, c.name, o.total));
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Orders with customer info ---");
    for (OrderIdCustomerNameTotal entry : result) {
        System.out.println("Order" + " " + entry.orderId + " " + "by" + " " + entry.customerName + " " + "- $" + " " + entry.total);
    }
    }
}
