import java.util.*;

class CustomerGJ {
    int id; String name;
    CustomerGJ(int id, String name) { this.id = id; this.name = name; }
}
class OrderGJ {
    int id; int customerId;
    OrderGJ(int id, int customerId) { this.id = id; this.customerId = customerId; }
}
class StatGJ {
    String name; int count;
    StatGJ(String name, int count) { this.name = name; this.count = count; }
}

public class GroupByJoin {
    public static void main(String[] args) {
        List<CustomerGJ> customers = Arrays.asList(
            new CustomerGJ(1, "Alice"),
            new CustomerGJ(2, "Bob")
        );
        List<OrderGJ> orders = Arrays.asList(
            new OrderGJ(100,1),
            new OrderGJ(101,1),
            new OrderGJ(102,2)
        );

        Map<String,Integer> counts = new LinkedHashMap<>();
        for (OrderGJ o : orders) {
            for (CustomerGJ c : customers) {
                if (c.id == o.customerId) {
                    counts.put(c.name, counts.getOrDefault(c.name,0)+1);
                }
            }
        }
        List<StatGJ> stats = new ArrayList<>();
        for (Map.Entry<String,Integer> e : counts.entrySet()) {
            stats.add(new StatGJ(e.getKey(), e.getValue()));
        }
        System.out.println("--- Orders per customer ---");
        for (StatGJ s : stats) {
            System.out.println(s.name + " orders: " + s.count);
        }
    }
}
