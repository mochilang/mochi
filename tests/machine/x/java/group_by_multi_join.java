// group_by_multi_join.mochi
import java.util.*;

class IdName {
    int id;
    String name;
    IdName(int id, String name) {
        this.id = id;
        this.name = name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdName other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.name, other.name);
    }
    @Override public int hashCode() {
        return Objects.hash(id, name);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdNation other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.nation, other.nation);
    }
    @Override public int hashCode() {
        return Objects.hash(id, nation);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PartSupplierCostQty other)) return false;
        return Objects.equals(this.part, other.part) && Objects.equals(this.supplier, other.supplier) && Objects.equals(this.cost, other.cost) && Objects.equals(this.qty, other.qty);
    }
    @Override public int hashCode() {
        return Objects.hash(part, supplier, cost, qty);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PartValue other)) return false;
        return Objects.equals(this.part, other.part) && Objects.equals(this.value, other.value);
    }
    @Override public int hashCode() {
        return Objects.hash(part, value);
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PartTotal other)) return false;
        return Objects.equals(this.part, other.part) && Objects.equals(this.total, other.total);
    }
    @Override public int hashCode() {
        return Objects.hash(part, total);
    }
    int size() { return 2; }
}
public class GroupByMultiJoin {
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    public static void main(String[] args) {
    List<IdName> nations = new ArrayList<>(Arrays.asList(new IdName(1, "A"), new IdName(2, "B")));
    List<IdNation> suppliers = new ArrayList<>(Arrays.asList(new IdNation(1, 1), new IdNation(2, 2)));
    List<PartSupplierCostQty> partsupp = new ArrayList<>(Arrays.asList(new PartSupplierCostQty(100, 1, 10.000000, 2), new PartSupplierCostQty(100, 2, 20.000000, 1), new PartSupplierCostQty(200, 1, 5.000000, 3)));
    List<PartValue> filtered = (new java.util.function.Supplier<List<PartValue>>(){public List<PartValue> get(){
    List<PartValue> res0 = new ArrayList<>();
    for (var ps : partsupp) {
        for (var s : suppliers) {
            if (!(Objects.equals(s.id, ps.supplier))) continue;
            for (var n : nations) {
                if (!(Objects.equals(n.id, s.nation))) continue;
                if (!(Objects.equals(n.name, "A"))) continue;
                res0.add(new PartValue(ps.part, ps.cost * ps.qty));
            }
        }
    }
    return res0;
}}).get();
    List<PartTotal> grouped = (new java.util.function.Supplier<List<PartTotal>>(){public List<PartTotal> get(){
    List<PartTotal> res1 = new ArrayList<>();
    Map<Integer,List<PartValue>> groups2 = new LinkedHashMap<>();
    for (var x : filtered) {
        var row3 = x;
        int key4 = x.part;
        List<PartValue> bucket5 = groups2.get(key4);
        if (bucket5 == null) { bucket5 = new ArrayList<>(); groups2.put(key4, bucket5); }
        bucket5.add(row3);
    }
    for (Map.Entry<Integer,List<PartValue>> __e : groups2.entrySet()) {
        int g_key = __e.getKey();
        Group<Integer,PartValue> g = new Group<>(g_key, __e.getValue());
        res1.add(new PartTotal(g.key, (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> res6 = new ArrayList<>();
    for (var r : g) {
        res6.add(r.value);
    }
    return res6;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
    }
    return res1;
}}).get();
    System.out.println(grouped);
    }
}
