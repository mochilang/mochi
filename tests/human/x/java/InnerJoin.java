import java.util.*;

class CustomerIJ {int id; String name; CustomerIJ(int id,String name){this.id=id;this.name=name;}}
class OrderIJ {int id; int customerId; int total; OrderIJ(int id,int customerId,int total){this.id=id;this.customerId=customerId;this.total=total;}}

public class InnerJoin {
    public static void main(String[] args) {
        List<CustomerIJ> customers = Arrays.asList(
            new CustomerIJ(1, "Alice"),
            new CustomerIJ(2, "Bob"),
            new CustomerIJ(3, "Charlie")
        );
        List<OrderIJ> orders = Arrays.asList(
            new OrderIJ(100,1,250),
            new OrderIJ(101,2,125),
            new OrderIJ(102,1,300),
            new OrderIJ(103,4,80)
        );
        System.out.println("--- Orders with customer info ---");
        for(OrderIJ o: orders){
            CustomerIJ c = null;
            for(CustomerIJ cu: customers) if(cu.id==o.customerId){c=cu;break;}
            if(c!=null){
                System.out.println("Order "+o.id+" by "+c.name+" - $"+o.total);
            }
        }
    }
}
