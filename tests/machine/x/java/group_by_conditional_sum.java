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
    int size() { return 3; }
}
class CatShare {
    Object cat;
    double share;
    CatShare(Object cat, double share) {
        this.cat = cat;
        this.share = share;
    }
    int size() { return 2; }
}
public class GroupByConditionalSum {
    public static void main(String[] args) {
    List<CatValFlag> items = new ArrayList<>(Arrays.asList(new CatValFlag("a", 10, true), new CatValFlag("a", 5, false), new CatValFlag("b", 20, true)));
    List<CatShare> result = (new java.util.function.Supplier<List<CatShare>>(){public List<CatShare> get(){
    List<CatShare> _res0 = new ArrayList<>();
    Map<String,List<CatValFlag>> _groups1 = new LinkedHashMap<>();
    for (var i : items) {
        var _row2 = i;
        String _key3 = i.cat;
        List<CatValFlag> _b4 = _groups1.get(_key3);
        if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
        _b4.add(_row2);
    }
    for (Map.Entry<String,List<CatValFlag>> __e : _groups1.entrySet()) {
        String g_key = __e.getKey();
        List<CatValFlag> g = __e.getValue();
        _res0.add(new CatShare(g_key, ((Number)(new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> _res5 = new ArrayList<>();
    for (var x : g) {
        _res5.add((x.flag ? x.val : 0));
    }
    return _res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue() / ((Number)(new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> _res6 = new ArrayList<>();
    for (var x : g) {
        _res6.add(x.val);
    }
    return _res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()).doubleValue()));
    }
    return _res0;
}}).get();
    System.out.println(result);
    }
}
