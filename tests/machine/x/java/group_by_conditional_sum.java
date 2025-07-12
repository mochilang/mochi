// group_by_conditional_sum.mochi
import java.util.*;

class CatValFlag {
    String cat;
    int val;
    boolean flag;
    CatValFlag(String cat, int val, boolean flag) {
        this.cat = cat;
        this.val = val;
        this.flag = flag;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CatValFlag other)) return false;
        return Objects.equals(this.cat, other.cat) && Objects.equals(this.val, other.val) && Objects.equals(this.flag, other.flag);
    }
    @Override public int hashCode() {
        return Objects.hash(cat, val, flag);
    }
    int size() { return 3; }
}
class CatShare {
    Object cat;
    double share;
    CatShare(Object cat, double share) {
        this.cat = cat;
        this.share = share;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CatShare other)) return false;
        return Objects.equals(this.cat, other.cat) && Objects.equals(this.share, other.share);
    }
    @Override public int hashCode() {
        return Objects.hash(cat, share);
    }
    int size() { return 2; }
}
public class GroupByConditionalSum {
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    public static void main(String[] args) {
    List<CatValFlag> items = new ArrayList<>(Arrays.asList(new CatValFlag("a", 10, true), new CatValFlag("a", 5, false), new CatValFlag("b", 20, true)));
    List<CatShare> result = (new java.util.function.Supplier<List<CatShare>>(){public List<CatShare> get(){
    List<CatShare> res0 = new ArrayList<>();
    Map<String,List<CatValFlag>> groups1 = new LinkedHashMap<>();
    for (var i : items) {
        var row2 = i;
        String key3 = i.cat;
        List<CatValFlag> bucket4 = groups1.get(key3);
        if (bucket4 == null) { bucket4 = new ArrayList<>(); groups1.put(key3, bucket4); }
        bucket4.add(row2);
    }
    for (Map.Entry<String,List<CatValFlag>> __e : groups1.entrySet()) {
        String g_key = __e.getKey();
        Group<String,CatValFlag> g = new Group<>(g_key, __e.getValue());
        res0.add(new CatShare(g.key, ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> res5 = new ArrayList<>();
    for (var x : g) {
        res5.add((x.flag ? x.val : 0));
    }
    return res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue() / ((Number)(new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> res6 = new ArrayList<>();
    for (var x : g) {
        res6.add(x.val);
    }
    return res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue()));
    }
    return res0;
}}).get();
    System.out.println(result);
    }
}
