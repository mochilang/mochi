import java.util.*;

class DataGI {
    String tag; int val;
    DataGI(String tag, int val){this.tag=tag; this.val=val;}
}
class ResultGI { String tag; int total; ResultGI(String tag,int total){this.tag=tag;this.total=total;} }

public class GroupItemsIteration {
    public static void main(String[] args) {
        List<DataGI> data = Arrays.asList(
            new DataGI("a",1), new DataGI("a",2), new DataGI("b",3)
        );
        Map<String,List<DataGI>> groups = new LinkedHashMap<>();
        for(DataGI d: data){
            groups.computeIfAbsent(d.tag,k->new ArrayList<>()).add(d);
        }
        List<ResultGI> tmp = new ArrayList<>();
        for(String tag: groups.keySet()){
            int total = 0;
            for(DataGI x: groups.get(tag)) total += x.val;
            tmp.add(new ResultGI(tag,total));
        }
        tmp.sort(Comparator.comparing(r->r.tag));
        System.out.print("[");
        for(int i=0;i<tmp.size();i++){
            ResultGI r=tmp.get(i);
            System.out.print("{tag="+r.tag+", total="+r.total+"}");
            if(i<tmp.size()-1) System.out.print(", ");
        }
        System.out.println("]");
    }
}
