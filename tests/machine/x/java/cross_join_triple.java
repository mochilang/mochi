import java.util.*;

class NLB {
    Integer n;
    String l;
    Boolean b;
    NLB(Integer n, String l, Boolean b) {
        this.n = n;
        this.l = l;
        this.b = b;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NLB other)) return false;
        return Objects.equals(this.n, other.n) && Objects.equals(this.l, other.l) && Objects.equals(this.b, other.b);
    }
    @Override public int hashCode() {
        return Objects.hash(n, l, b);
    }
    int size() { return 3; }
}
public class CrossJoinTriple {
    public static void main(String[] args) {
    List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2));
    List<String> letters = new ArrayList<>(Arrays.asList("A", "B"));
    List<Boolean> bools = new ArrayList<>(Arrays.asList(true, false));
    List<NLB> combos = (new java.util.function.Supplier<List<NLB>>(){public List<NLB> get(){
    List<NLB> _res0 = new ArrayList<>();
    for (var n : nums) {
        for (var l : letters) {
            for (var b : bools) {
                _res0.add(new NLB(n, l, b));
            }
        }
    }
    return _res0;
}}).get();
    System.out.println("--- Cross Join of three lists ---");
    for (NLB c : combos) {
        System.out.println(c.n + " " + c.l + " " + c.b);
    }
    }
}
