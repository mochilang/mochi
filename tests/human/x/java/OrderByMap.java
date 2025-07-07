import java.util.*;

class DataOM { int a; int b; DataOM(int a,int b){this.a=a;this.b=b;} }

public class OrderByMap {
    public static void main(String[] args) {
        List<DataOM> data = Arrays.asList(
            new DataOM(1,2),
            new DataOM(1,1),
            new DataOM(0,5)
        );
        data.sort(Comparator.<DataOM>comparingInt(d->d.a).thenComparingInt(d->d.b));
        for(DataOM d: data){
            System.out.println("{"+"a="+d.a+", b="+d.b+"}");
        }
    }
}
