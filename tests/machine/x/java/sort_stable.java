// sort_stable.mochi
import java.util.*;

class NV {
    int n;
    String v;
    NV(int n, String v) {
        this.n = n;
        this.v = v;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NV other)) return false;
        return Objects.equals(this.n, other.n) && Objects.equals(this.v, other.v);
    }
    @Override public int hashCode() {
        return Objects.hash(n, v);
    }
    int size() { return 2; }
}
public class SortStable {
    public static void main(String[] args) {
    List<NV> items = new ArrayList<>(Arrays.asList(new NV(1, "a"), new NV(1, "b"), new NV(2, "c")));
    List<String> result = (new java.util.function.Supplier<List<String>>(){public List<String> get(){
    List<String> res0 = new ArrayList<>();
    for (var i : items) {
        res0.add(i.v);
    }
    return res0;
}}).get();
    System.out.println(result);
    }
}
