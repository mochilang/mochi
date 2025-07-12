// group_by_sort.mochi
import java.util.*;

class CatVal {
    String cat;
    int val;
    CatVal(String cat, int val) {
        this.cat = cat;
        this.val = val;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CatVal other)) return false;
        return Objects.equals(this.cat, other.cat) && Objects.equals(this.val, other.val);
    }
    @Override public int hashCode() {
        return Objects.hash(cat, val);
    }
    int size() { return 2; }
}
class CatTotal {
    Object cat;
    int total;
    CatTotal(Object cat, int total) {
        this.cat = cat;
        this.total = total;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CatTotal other)) return false;
        return Objects.equals(this.cat, other.cat) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(cat, total);
    }
    int size() { return 2; }
}
public class GroupBySort {
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    public static void main(String[] args) {
    List<CatVal> items = new ArrayList<>(Arrays.asList(new CatVal("a", 3), new CatVal("a", 1), new CatVal("b", 5), new CatVal("b", 2)));
    List<CatTotal> grouped = (new java.util.function.Supplier<List<CatTotal>>(){public List<CatTotal> get(){
    List<CatTotal> res0 = new ArrayList<>();
    Map<String,List<CatVal>> groups1 = new LinkedHashMap<>();
    for (var i : items) {
        var row2 = i;
        String key3 = i.cat;
        List<CatVal> bucket4 = groups1.get(key3);
        if (bucket4 == null) { bucket4 = new ArrayList<>(); groups1.put(key3, bucket4); }
        bucket4.add(row2);
    }
    for (Map.Entry<String,List<CatVal>> __e : groups1.entrySet()) {
        String g_key = __e.getKey();
        Group<String,CatVal> g = new Group<>(g_key, __e.getValue());
        res0.add(new CatTotal(g.key, (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> res5 = new ArrayList<>();
    for (var x : g) {
        res5.add(x.val);
    }
    return res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
    }
    return res0;
}}).get();
    System.out.println(grouped);
    }
}
