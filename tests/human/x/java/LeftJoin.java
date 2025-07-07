import java.util.*;

class CustomerLJ {int id; String name; CustomerLJ(int id,String name){this.id=id;this.name=name;}}
class OrderLJ {int id; int customerId; int total; OrderLJ(int id,int customerId,int total){this.id=id;this.customerId=customerId;this.total=total;}}

public class LeftJoin {
    public static void main(String[] args) {
        List<CustomerLJ> customers=Arrays.asList(
            new CustomerLJ(1,"Alice"),
            new CustomerLJ(2,"Bob")
        );
        List<OrderLJ> orders=Arrays.asList(
            new OrderLJ(100,1,250),
            new OrderLJ(101,3,80)
        );
        System.out.println("--- Left Join ---");
        for(OrderLJ o:orders){
            CustomerLJ c=null; for(CustomerLJ x:customers) if(x.id==o.customerId){c=x;break;}
            String cust = c!=null ? c.name : "null";
            System.out.println("Order "+o.id+" customer "+cust+" total "+o.total);
        }
    }
}
