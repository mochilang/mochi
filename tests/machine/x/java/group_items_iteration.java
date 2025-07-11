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
    public static void main(String[] args) {
    List<TagVal> data = new ArrayList<>(Arrays.asList(new TagVal("a", 1), new TagVal("a", 2), new TagVal("b", 3)));
    List<Object> groups = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> _res0 = new ArrayList<>();
    Map<String,List<TagVal>> _groups1 = new LinkedHashMap<>();
    for (var d : data) {
        var _row2 = d;
        String _key3 = d.tag;
        List<TagVal> _b4 = _groups1.get(_key3);
        if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
        _b4.add(_row2);
    }
    for (Map.Entry<String,List<TagVal>> __e : _groups1.entrySet()) {
        String g_key = __e.getKey();
        List<TagVal> g = __e.getValue();
        _res0.add(new LinkedHashMap<>(Map.ofEntries(Map.entry("key", g_key), Map.entry("items", g))));
    }
    return _res0;
}}).get();
    List<Object> tmp = new ArrayList<>(Arrays.asList());
    for (Object g : groups) {
        int total = 0;
        for (Object x : (List)((Map)g).get("items")) {
            total = (int)(total + ((Number)((Map)x).get("val")).doubleValue());
        }
        tmp = append(tmp, new TagTotal(((Map)g).get("key"), total));
    }
    List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> _res5 = new ArrayList<>();
    for (var r : tmp) {
        _res5.add(r);
    }
    return _res5;
}}).get();
    System.out.println(result);
    }
}
