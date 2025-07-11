// left_join_multi.mochi
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
class OrderIdNameItem {
    int orderId;
    String name;
    OrderIdSku item;
    OrderIdNameItem(int orderId, String name, OrderIdSku item) {
        this.orderId = orderId;
        this.name = name;
        this.item = item;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderIdNameItem other)) return false;
        return Objects.equals(this.orderId, other.orderId) && Objects.equals(this.name, other.name) && Objects.equals(this.item, other.item);
    }
    @Override public int hashCode() {
        return Objects.hash(orderId, name, item);
    }
    int size() { return 3; }
}
public class LeftJoinMulti {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob")));
    List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 2)));
    List<OrderIdSku> items = new ArrayList<>(Arrays.asList(new OrderIdSku(100, "a")));
    List<OrderIdNameItem> result = (new java.util.function.Supplier<List<OrderIdNameItem>>(){public List<OrderIdNameItem> get(){
    List<OrderIdNameItem> res0 = new ArrayList<>();
    for (var o : orders) {
        for (var c : customers) {
            if (!(Objects.equals(o.customerId, c.id))) continue;
            List<OrderIdSku> tmp1 = new ArrayList<>();
            for (var it2 : items) {
                var i = it2;
                if (!(Objects.equals(o.id, i.orderId))) continue;
                tmp1.add(it2);
            }
            if (tmp1.isEmpty()) tmp1.add(null);
            for (var i : tmp1) {
                res0.add(new OrderIdNameItem(o.id, c.name, i));
            }
        }
    }
    return res0;
}}).get();
    System.out.println("--- Left Join Multi ---");
    for (OrderIdNameItem r : result) {
        System.out.println(r.orderId + " " + r.name + " " + r.item);
    }
    }
}
