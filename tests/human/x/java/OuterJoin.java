import java.util.*;

class CustomerOJ {int id; String name; CustomerOJ(int id,String name){this.id=id;this.name=name;}}
class OrderOJ {int id; int customerId; int total; OrderOJ(int id,int customerId,int total){this.id=id;this.customerId=customerId;this.total=total;}}

public class OuterJoin {
    public static void main(String[] args) {
        List<CustomerOJ> customers = Arrays.asList(
            new CustomerOJ(1,"Alice"),
            new CustomerOJ(2,"Bob"),
            new CustomerOJ(3,"Charlie"),
            new CustomerOJ(4,"Diana")
        );
        List<OrderOJ> orders = Arrays.asList(
            new OrderOJ(100,1,250),
            new OrderOJ(101,2,125),
            new OrderOJ(102,1,300),
            new OrderOJ(103,5,80)
        );
        System.out.println("--- Outer Join using syntax ---");
        Set<Integer> matchedCustomers = new HashSet<>();
        for(OrderOJ o:orders){
            CustomerOJ c=null; for(CustomerOJ cu:customers) if(cu.id==o.customerId){c=cu;matchedCustomers.add(c.id);break;}
            if(c!=null){
                System.out.println("Order "+o.id+" by "+c.name+" - $"+o.total);
            } else {
                System.out.println("Order "+o.id+" by Unknown - $"+o.total);
            }
        }
        for(CustomerOJ c:customers){
            if(!matchedCustomers.contains(c.id)){
                System.out.println("Customer "+c.name+" has no orders");
            }
        }
    }
}
