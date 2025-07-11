import java.util.*;

public class ListSetOps {
    static <T> List<T> union_all(List<T> a, List<T> b) {
        List<T> res = new ArrayList<>(a);
        res.addAll(b);
        return res;
    }
    static <T> List<T> union(List<T> a, List<T> b) {
        LinkedHashSet<T> s = new LinkedHashSet<>(a);
        s.addAll(b);
        return new ArrayList<>(s);
    }
    static <T> List<T> except(List<T> a, List<T> b) {
        List<T> res = new ArrayList<>();
        for (T x : a) if (!b.contains(x)) res.add(x);
        return res;
    }
    static <T> List<T> intersect(List<T> a, List<T> b) {
        List<T> res = new ArrayList<>();
        for (T x : a) if (b.contains(x) && !res.contains(x)) res.add(x);
        return res;
    }
    public static void main(String[] args) {
    System.out.println(union(Arrays.asList(1, 2), Arrays.asList(2, 3)));
    System.out.println(except(Arrays.asList(1, 2, 3), Arrays.asList(2)));
    System.out.println(intersect(Arrays.asList(1, 2, 3), Arrays.asList(2, 4)));
    System.out.println(union_all(Arrays.asList(1, 2), Arrays.asList(2, 3)).size());
    }
}
