import java.util.*;

class NationGMS {int key; String name; NationGMS(int k,String n){key=k;name=n;}}
class CustomerGMS {int custkey; String name; double acctbal; int nationkey; String address; String phone; String comment; CustomerGMS(int k,String n,double a,int nk,String ad,String ph,String c){custkey=k;name=n;acctbal=a;nationkey=nk;address=ad;phone=ph;comment=c;}}
class OrderGMS {int orderkey; int custkey; String orderdate; OrderGMS(int k,int c,String d){orderkey=k;custkey=c;orderdate=d;}}
class LineItemGMS {int orderkey; String returnflag; double extendedprice; double discount; LineItemGMS(int k,String rf,double ep,double d){orderkey=k;returnflag=rf;extendedprice=ep;discount=d;}}

public class GroupByMultiJoinSort {
    static class Row {CustomerGMS c; OrderGMS o; LineItemGMS l; NationGMS n; Row(CustomerGMS c,OrderGMS o,LineItemGMS l,NationGMS n){this.c=c;this.o=o;this.l=l;this.n=n;}}
    public static void main(String[] args) {
        List<NationGMS> nation = Arrays.asList(new NationGMS(1,"BRAZIL"));
        List<CustomerGMS> customer = Arrays.asList(new CustomerGMS(1,"Alice",100.0,1,"123 St","123-456","Loyal"));
        List<OrderGMS> orders = Arrays.asList(
            new OrderGMS(1000,1,"1993-10-15"),
            new OrderGMS(2000,1,"1994-01-02")
        );
        List<LineItemGMS> lineitem = Arrays.asList(
            new LineItemGMS(1000,"R",1000.0,0.1),
            new LineItemGMS(2000,"N",500.0,0.0)
        );
        String start="1993-10-01";
        String end="1994-01-01";
        List<Row> rows=new ArrayList<>();
        for(CustomerGMS c:customer){
            for(OrderGMS o:orders) if(o.custkey==c.custkey){
                for(LineItemGMS l:lineitem) if(l.orderkey==o.orderkey){
                    for(NationGMS n:nation) if(n.key==c.nationkey){
                        if(o.orderdate.compareTo(start)>=0 && o.orderdate.compareTo(end)<0 && "R".equals(l.returnflag)){
                            rows.add(new Row(c,o,l,n));
                        }
                    }
                }
            }
        }
        Map<String,List<Row>> groups=new LinkedHashMap<>();
        for(Row r:rows){
            String key=r.c.custkey+"|"+r.c.name+"|"+r.c.acctbal+"|"+r.c.address+"|"+r.c.phone+"|"+r.c.comment+"|"+r.n.name;
            groups.computeIfAbsent(key,k->new ArrayList<>()).add(r);
        }
        class Result {int custkey; String name; double revenue; double acctbal; String nation; String address; String phone; String comment;}
        List<Result> result=new ArrayList<>();
        for(Map.Entry<String,List<Row>> e:groups.entrySet()){
            String[] parts=e.getKey().split("\\|");
            double revenue=0.0;
            for(Row r:e.getValue()) revenue+=r.l.extendedprice*(1-r.l.discount);
            Result res=new Result();
            res.custkey=Integer.parseInt(parts[0]);
            res.name=parts[1];
            res.revenue=revenue;
            res.acctbal=Double.parseDouble(parts[2]);
            res.nation=parts[6];
            res.address=parts[3];
            res.phone=parts[4];
            res.comment=parts[5];
            result.add(res);
        }
        result.sort((a,b)->Double.compare(b.revenue,a.revenue));
        for(Result r:result){
            System.out.println(r.name+" revenue:"+r.revenue);
        }
    }
}
