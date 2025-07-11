// group_items_iteration.mochi
import java.util.*;

class TagVal {
    String tag;
    int val;
    TagVal(String tag, int val) {
        this.tag = tag;
        this.val = val;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TagVal other)) return false;
        return Objects.equals(this.tag, other.tag) && Objects.equals(this.val, other.val);
    }
    @Override public int hashCode() {
        return Objects.hash(tag, val);
    }
    int size() { return 2; }
}
class TagTotal {
    Object tag;
    int total;
    TagTotal(Object tag, int total) {
        this.tag = tag;
        this.total = total;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TagTotal other)) return false;
        return Objects.equals(this.tag, other.tag) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(tag, total);
    }
    int size() { return 2; }
}
public class GroupItemsIteration {
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    public static void main(String[] args) {
    List<TagVal> data = new ArrayList<>(Arrays.asList(new TagVal("a", 1), new TagVal("a", 2), new TagVal("b", 3)));
    List<Group<String,TagVal>> groups = (new java.util.function.Supplier<List<Group<String,TagVal>>>(){public List<Group<String,TagVal>> get(){
    List<Group<String,TagVal>> res0 = new ArrayList<>();
    Map<String,List<TagVal>> groups1 = new LinkedHashMap<>();
    for (var d : data) {
        var row2 = d;
        String key3 = d.tag;
        List<TagVal> bucket4 = groups1.get(key3);
        if (bucket4 == null) { bucket4 = new ArrayList<>(); groups1.put(key3, bucket4); }
        bucket4.add(row2);
    }
    for (Map.Entry<String,List<TagVal>> __e : groups1.entrySet()) {
        String g_key = __e.getKey();
        Group<String,TagVal> g = new Group<>(g_key, __e.getValue());
        res0.add(g);
    }
    return res0;
}}).get();
    List<Object> tmp = new ArrayList<>(Arrays.asList());
    for (Group<String,TagVal> g : groups) {
        int total = 0;
        for (TagVal x : g.items) {
            total = (int)(total + x.val);
        }
        tmp.add(new TagTotal(g.key, total));
    }
    List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> res5 = new ArrayList<>();
    for (var r : tmp) {
        res5.add(r);
    }
    return res5;
}}).get();
    System.out.println(result);
    }
}
