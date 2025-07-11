// order_by_map.mochi
import java.util.*;

class AB {
    int a;
    int b;
    AB(int a, int b) {
        this.a = a;
        this.b = b;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AB other)) return false;
        return Objects.equals(this.a, other.a) && Objects.equals(this.b, other.b);
    }
    @Override public int hashCode() {
        return Objects.hash(a, b);
    }
    int size() { return 2; }
}
public class OrderByMap {
    public static void main(String[] args) {
    List<AB> data = new ArrayList<>(Arrays.asList(new AB(1, 2), new AB(1, 1), new AB(0, 5)));
    List<AB> sorted = (new java.util.function.Supplier<List<AB>>(){public List<AB> get(){
    List<AB> res0 = new ArrayList<>();
    for (var x : data) {
        res0.add(x);
    }
    return res0;
}}).get();
    System.out.println(sorted);
    }
}
