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
class IdCustomerId {
    int id;
    int customerId;
    IdCustomerId(int id, int customerId) {
        this.id = id;
        this.customerId = customerId;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdCustomerId other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.customerId, other.customerId);
    }
    @Override public int hashCode() {
        return Objects.hash(id, customerId);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderIdSku other)) return false;
        return Objects.equals(this.orderId, other.orderId) && Objects.equals(this.sku, other.sku);
    }
    @Override public int hashCode() {
        return Objects.hash(orderId, sku);
    }
    int size() { return 2; }
}
class NameSku {
    String name;
    String sku;
    NameSku(String name, String sku) {
        this.name = name;
        this.sku = sku;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NameSku other)) return false;
        return Objects.equals(this.name, other.name) && Objects.equals(this.sku, other.sku);
    }
    @Override public int hashCode() {
        return Objects.hash(name, sku);
    }
    int size() { return 2; }
}
public class JoinMulti {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
    List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 2)));
    List<OrderIdSku> items = new ArrayList<>(Arrays.asList(new OrderIdSku(100, "a"), new OrderIdSku(101, "b")));
    List<NameSku> result = (new java.util.function.Supplier<List<NameSku>>(){public List<NameSku> get(){
    List<NameSku> _res0 = new ArrayList<>();
    for (var o : orders) {
        for (var c : customers) {
            if (!(Objects.equals(o.customerId, c.id))) continue;
            for (var i : items) {
                if (!(Objects.equals(o.id, i.orderId))) continue;
                _res0.add(new NameSku(c.name, i.sku));
            }
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Multi Join ---");
    for (NameSku r : result) {
        System.out.println(r.name + " " + "bought item" + " " + r.sku);
    }
    }
}
