import java.util.*;

class NationGBMJ {int id; String name; NationGBMJ(int id,String name){this.id=id;this.name=name;}}
class SupplierGBMJ {int id; int nation; SupplierGBMJ(int id,int nation){this.id=id;this.nation=nation;}}
class PartSuppGBMJ {int part; int supplier; double cost; int qty; PartSuppGBMJ(int p,int s,double c,int q){part=p;supplier=s;cost=c;qty=q;}}

public class GroupByMultiJoin {
    public static void main(String[] args) {
        List<NationGBMJ> nations = Arrays.asList(
            new NationGBMJ(1,"A"),
            new NationGBMJ(2,"B")
        );
        List<SupplierGBMJ> suppliers = Arrays.asList(
            new SupplierGBMJ(1,1),
            new SupplierGBMJ(2,2)
        );
        List<PartSuppGBMJ> partsupp = Arrays.asList(
            new PartSuppGBMJ(100,1,10.0,2),
            new PartSuppGBMJ(100,2,20.0,1),
            new PartSuppGBMJ(200,1,5.0,3)
        );

        Map<Integer,Double> totals = new LinkedHashMap<>();
        for(PartSuppGBMJ ps: partsupp){
            SupplierGBMJ s = null;
            for(SupplierGBMJ x: suppliers) if(x.id==ps.supplier){s=x;break;}
            NationGBMJ n = null;
            if(s!=null) for(NationGBMJ y: nations) if(y.id==s.nation){n=y;break;}
            if(n!=null && "A".equals(n.name)){
                double value = ps.cost * ps.qty;
                totals.put(ps.part, totals.getOrDefault(ps.part,0.0)+value);
            }
        }
        List<String> result = new ArrayList<>();
        for(Map.Entry<Integer,Double> e: totals.entrySet()){
            result.add("{part:"+e.getKey()+", total:"+e.getValue()+"}");
        }
        System.out.println(result);
    }
}
