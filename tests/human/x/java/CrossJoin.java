import java.util.*;

class Customer {
    int id; String name;
    Customer(int id, String name) { this.id = id; this.name = name; }
}
class Order {
    int id; int customerId; int total;
    Order(int id, int customerId, int total) { this.id = id; this.customerId = customerId; this.total = total; }
}

public class CrossJoin {
    public static void main(String[] args) {
        List<Customer> customers = Arrays.asList(
            new Customer(1, "Alice"),
            new Customer(2, "Bob"),
            new Customer(3, "Charlie")
        );
        List<Order> orders = Arrays.asList(
            new Order(100, 1, 250),
            new Order(101, 2, 125),
            new Order(102, 1, 300)
        );
        System.out.println("--- Cross Join: All order-customer pairs ---");
        for (Order o : orders) {
            for (Customer c : customers) {
                System.out.println("Order " + o.id + "(customerId: " + o.customerId +
                                   ", total: $" + o.total + ") paired with " + c.name);
            }
        }
    }
}
