import java.util.*;

class CustomerGLJ {
    int id; String name;
    CustomerGLJ(int id, String name) { this.id = id; this.name = name; }
}

class OrderGLJ {
    int id; int customerId;
    OrderGLJ(int id, int customerId) { this.id = id; this.customerId = customerId; }
}

public class GroupByLeftJoin {
    public static void main(String[] args) {
        List<CustomerGLJ> customers = Arrays.asList(
            new CustomerGLJ(1, "Alice"),
            new CustomerGLJ(2, "Bob"),
            new CustomerGLJ(3, "Charlie")
        );
        List<OrderGLJ> orders = Arrays.asList(
            new OrderGLJ(100, 1),
            new OrderGLJ(101, 1),
            new OrderGLJ(102, 2)
        );

        System.out.println("--- Group Left Join ---");
        for (CustomerGLJ c : customers) {
            int count = 0;
            for (OrderGLJ o : orders) {
                if (o.customerId == c.id) count++;
            }
            System.out.println(c.name + " orders: " + count);
        }
    }
}
