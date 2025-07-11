import java.util.*;

class IdName {
    int id;
    String name;
    IdName(int id, String name) {
        this.id = id;
        this.name = name;
    }
    int size() { return 2; }
}
class IdNation {
    int id;
    int nation;
    IdNation(int id, int nation) {
        this.id = id;
        this.nation = nation;
    }
    int size() { return 2; }
}
class PartSupplierCostQty {
    int part;
    int supplier;
    double cost;
    int qty;
    PartSupplierCostQty(int part, int supplier, double cost, int qty) {
        this.part = part;
        this.supplier = supplier;
        this.cost = cost;
        this.qty = qty;
    }
    int size() { return 4; }
}
class PartValue {
    int part;
    double value;
    PartValue(int part, double value) {
        this.part = part;
        this.value = value;
    }
    int size() { return 2; }
}
class PartTotal {
    Object part;
    int total;
    PartTotal(Object part, int total) {
        this.part = part;
        this.total = total;
    }
    int size() { return 2; }
}
public class GroupByMultiJoin {
    public static void main(String[] args) {
    List<IdName> nations = new ArrayList<>(Arrays.asList(new IdName(1, "A"), new IdName(2, "B")));
    List<IdNation> suppliers = new ArrayList<>(Arrays.asList(new IdNation(1, 1), new IdNation(2, 2)));
    List<PartSupplierCostQty> partsupp = new ArrayList<>(Arrays.asList(new PartSupplierCostQty(100, 1, 10.000000, 2), new PartSupplierCostQty(100, 2, 20.000000, 1), new PartSupplierCostQty(200, 1, 5.000000, 3)));
    List<PartValue> filtered = (new java.util.function.Supplier<List<PartValue>>(){public List<PartValue> get(){
    List<PartValue> _res0 = new ArrayList<>();
    for (var ps : partsupp) {
        for (var s : suppliers) {
            if (!(Objects.equals(s.id, ps.supplier))) continue;
            for (var n : nations) {
                if (!(Objects.equals(n.id, s.nation))) continue;
                if (!(Objects.equals(n.name, "A") != null)) continue;
                _res0.add(new PartValue(ps.part, ps.cost * ps.qty));
            }
        }
    }
    return _res0;
}}).get();
    List<PartTotal> grouped = (new java.util.function.Supplier<List<PartTotal>>(){public List<PartTotal> get(){
    List<PartTotal> _res1 = new ArrayList<>();
    Map<Integer,List<PartValue>> _groups2 = new LinkedHashMap<>();
    for (var x : filtered) {
        var _row3 = x;
        int _key4 = x.part;
        List<PartValue> _b5 = _groups2.get(_key4);
        if (_b5 == null) { _b5 = new ArrayList<>(); _groups2.put(_key4, _b5); }
        _b5.add(_row3);
    }
    for (Map.Entry<int,List<PartValue>> __e : _groups2.entrySet()) {
        int g_key = __e.getKey();
        List<PartValue> g = __e.getValue();
        _res1.add(new PartTotal(g_key, (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> _res6 = new ArrayList<>();
    for (var r : g) {
        _res6.add(r.value);
    }
    return _res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
    }
    return _res1;
}}).get();
    System.out.println(grouped);
    }
}
