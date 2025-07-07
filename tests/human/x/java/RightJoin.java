import java.util.*;

class CustomerRJ {int id; String name; CustomerRJ(int id,String name){this.id=id;this.name=name;}}
class OrderRJ {int id; int customerId; int total; OrderRJ(int id,int customerId,int total){this.id=id;this.customerId=customerId;this.total=total;}}

public class RightJoin {
    public static void main(String[] args) {
        List<CustomerRJ> customers=Arrays.asList(
            new CustomerRJ(1,"Alice"),
            new CustomerRJ(2,"Bob"),
            new CustomerRJ(3,"Charlie"),
            new CustomerRJ(4,"Diana")
        );
        List<OrderRJ> orders=Arrays.asList(
            new OrderRJ(100,1,250),
            new OrderRJ(101,2,125),
            new OrderRJ(102,1,300)
        );
        System.out.println("--- Right Join using syntax ---");
        for(CustomerRJ c:customers){
            boolean found=false;
            for(OrderRJ o:orders){
                if(o.customerId==c.id){
                    System.out.println("Customer "+c.name+" has order "+o.id+" - $"+o.total);
                    found=true;
                }
            }
            if(!found){
                System.out.println("Customer "+c.name+" has no orders");
            }
        }
    }
}
