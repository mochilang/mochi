import java.util.*;

class CustomerLJM {int id; String name; CustomerLJM(int id,String name){this.id=id;this.name=name;}}
class OrderLJM {int id; int customerId; OrderLJM(int id,int customerId){this.id=id;this.customerId=customerId;}}
class ItemLJM {int orderId; String sku; ItemLJM(int orderId,String sku){this.orderId=orderId;this.sku=sku;}}

public class LeftJoinMulti {
    public static void main(String[] args) {
        List<CustomerLJM> customers=Arrays.asList(
            new CustomerLJM(1,"Alice"),
            new CustomerLJM(2,"Bob")
        );
        List<OrderLJM> orders=Arrays.asList(
            new OrderLJM(100,1),
            new OrderLJM(101,2)
        );
        List<ItemLJM> items=Arrays.asList(
            new ItemLJM(100,"a")
        );
        System.out.println("--- Left Join Multi ---");
        for(OrderLJM o:orders){
            CustomerLJM c=null; for(CustomerLJM x:customers) if(x.id==o.customerId){c=x;break;}
            ItemLJM item=null; for(ItemLJM i:items) if(i.orderId==o.id){item=i;break;}
            System.out.println(o.id+" "+c.name+" "+(item!=null?item.sku:null));
        }
    }
}
