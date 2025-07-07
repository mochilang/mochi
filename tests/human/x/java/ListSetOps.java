import java.util.*;

public class ListSetOps {
    static List<Integer> union(List<Integer> a, List<Integer> b) {
        Set<Integer> s = new LinkedHashSet<>(a);
        s.addAll(b);
        return new ArrayList<>(s);
    }
    static List<Integer> except(List<Integer> a, List<Integer> b) {
        List<Integer> res = new ArrayList<>();
        for(int x : a) if(!b.contains(x)) res.add(x);
        return res;
    }
    static List<Integer> intersect(List<Integer> a, List<Integer> b) {
        List<Integer> res = new ArrayList<>();
        for(int x : a) if(b.contains(x) && !res.contains(x)) res.add(x);
        return res;
    }
    public static void main(String[] args) {
        System.out.println(union(Arrays.asList(1,2), Arrays.asList(2,3)));
        System.out.println(except(Arrays.asList(1,2,3), Arrays.asList(2)));
        System.out.println(intersect(Arrays.asList(1,2,3), Arrays.asList(2,4)));
        System.out.println(union(Arrays.asList(1,2), Arrays.asList(2,3)).size());
    }
}
