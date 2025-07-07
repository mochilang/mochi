import java.util.*;

class CustomerJM {int id; String name; CustomerJM(int id,String name){this.id=id;this.name=name;}}
class OrderJM {int id; int customerId; OrderJM(int id,int customerId){this.id=id;this.customerId=customerId;}}
class ItemJM {int orderId; String sku; ItemJM(int orderId,String sku){this.orderId=orderId;this.sku=sku;}}

public class JoinMulti {
    public static void main(String[] args) {
        List<CustomerJM> customers=Arrays.asList(
            new CustomerJM(1,"Alice"),
            new CustomerJM(2,"Bob")
        );
        List<OrderJM> orders=Arrays.asList(
            new OrderJM(100,1),
            new OrderJM(101,2)
        );
        List<ItemJM> items=Arrays.asList(
            new ItemJM(100,"a"),
            new ItemJM(101,"b")
        );
        List<String> result=new ArrayList<>();
        for(OrderJM o:orders){
            CustomerJM c=null; for(CustomerJM x:customers) if(x.id==o.customerId){c=x;break;}
            if(c!=null){
                for(ItemJM i:items) if(i.orderId==o.id){
                    result.add(c.name+" bought item "+i.sku);
                }
            }
        }
        System.out.println("--- Multi Join ---");
        for(String r:result) System.out.println(r);
    }
}
