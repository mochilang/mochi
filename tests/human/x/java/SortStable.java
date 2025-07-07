import java.util.*;

class ItemSS { int n; String v; ItemSS(int n,String v){this.n=n;this.v=v;} }

public class SortStable {
    public static void main(String[] args) {
        List<ItemSS> items = Arrays.asList(
            new ItemSS(1,"a"),
            new ItemSS(1,"b"),
            new ItemSS(2,"c")
        );
        items.sort(Comparator.comparingInt(i -> i.n));
        List<String> result = new ArrayList<>();
        for(ItemSS i: items) result.add(i.v);
        System.out.println(result);
    }
}
