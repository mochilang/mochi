import java.util.*;

class A {
    int a;
    A(int a) {
        this.a = a;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof A other)) return false;
        return Objects.equals(this.a, other.a);
    }
    @Override public int hashCode() {
        return Objects.hash(a);
    }
    int size() { return 1; }
}
public class InOperatorExtended {
    static boolean inOp(Object item, Object collection) {
        if (collection instanceof Map<?,?> m) return m.containsKey(item);
        if (collection instanceof Collection<?> c) return c.contains(item);
        if (collection instanceof String s) return s.contains(String.valueOf(item));
        return false;
    }
    public static void main(String[] args) {
    List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
    List<Integer> ys = (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> _res0 = new ArrayList<>();
    for (var x : xs) {
        if (!(Objects.equals(x % 2, 1))) continue;
        _res0.add(x);
    }
    return _res0;
}}).get();
    System.out.println(ys.contains(1));
    System.out.println(ys.contains(2));
    A m = new A(1);
    System.out.println(inOp("a", m));
    System.out.println(inOp("b", m));
    String s = "hello";
    System.out.println(s.contains(String.valueOf("ell")));
    System.out.println(s.contains(String.valueOf("foo")));
    }
}
