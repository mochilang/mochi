import java.util.*;

class ItemGBS {String cat; int val; ItemGBS(String c,int v){cat=c;val=v;}}

public class GroupBySort {
    public static void main(String[] args) {
        List<ItemGBS> items=Arrays.asList(
            new ItemGBS("a",3),
            new ItemGBS("a",1),
            new ItemGBS("b",5),
            new ItemGBS("b",2)
        );
        Map<String,Integer> totals=new LinkedHashMap<>();
        for(ItemGBS i:items){
            totals.put(i.cat, totals.getOrDefault(i.cat,0)+i.val);
        }
        List<Map.Entry<String,Integer>> result=new ArrayList<>(totals.entrySet());
        result.sort((a,b)->Integer.compare(b.getValue(),a.getValue()));
        for(Map.Entry<String,Integer> e:result){
            System.out.println("{cat:"+e.getKey()+", total:"+e.getValue()+"}");
        }
    }
}
