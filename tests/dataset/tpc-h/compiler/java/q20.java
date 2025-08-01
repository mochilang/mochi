// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:00:00Z
// q20.mochi
import java.util.*;

class Nation {
    int n_nationkey;
    String n_name;
    Nation(int n_nationkey, String n_name) {
        this.n_nationkey = n_nationkey;
        this.n_name = n_name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Nation other)) return false;
        return Objects.equals(this.n_nationkey, other.n_nationkey) && Objects.equals(this.n_name, other.n_name);
    }
    @Override public int hashCode() {
        return Objects.hash(n_nationkey, n_name);
    }
    int size() { return 2; }
}
class Supplier {
    int s_suppkey;
    String s_name;
    String s_address;
    int s_nationkey;
    Supplier(int s_suppkey, String s_name, String s_address, int s_nationkey) {
        this.s_suppkey = s_suppkey;
        this.s_name = s_name;
        this.s_address = s_address;
        this.s_nationkey = s_nationkey;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Supplier other)) return false;
        return Objects.equals(this.s_suppkey, other.s_suppkey) && Objects.equals(this.s_name, other.s_name) && Objects.equals(this.s_address, other.s_address) && Objects.equals(this.s_nationkey, other.s_nationkey);
    }
    @Override public int hashCode() {
        return Objects.hash(s_suppkey, s_name, s_address, s_nationkey);
    }
    int size() { return 4; }
}
class Part {
    int p_partkey;
    String p_name;
    Part(int p_partkey, String p_name) {
        this.p_partkey = p_partkey;
        this.p_name = p_name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Part other)) return false;
        return Objects.equals(this.p_partkey, other.p_partkey) && Objects.equals(this.p_name, other.p_name);
    }
    @Override public int hashCode() {
        return Objects.hash(p_partkey, p_name);
    }
    int size() { return 2; }
}
class Partsupp {
    int ps_partkey;
    int ps_suppkey;
    int ps_availqty;
    Partsupp(int ps_partkey, int ps_suppkey, int ps_availqty) {
        this.ps_partkey = ps_partkey;
        this.ps_suppkey = ps_suppkey;
        this.ps_availqty = ps_availqty;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Partsupp other)) return false;
        return Objects.equals(this.ps_partkey, other.ps_partkey) && Objects.equals(this.ps_suppkey, other.ps_suppkey) && Objects.equals(this.ps_availqty, other.ps_availqty);
    }
    @Override public int hashCode() {
        return Objects.hash(ps_partkey, ps_suppkey, ps_availqty);
    }
    int size() { return 3; }
}
class Lineitem {
    int l_partkey;
    int l_suppkey;
    int l_quantity;
    String l_shipdate;
    Lineitem(int l_partkey, int l_suppkey, int l_quantity, String l_shipdate) {
        this.l_partkey = l_partkey;
        this.l_suppkey = l_suppkey;
        this.l_quantity = l_quantity;
        this.l_shipdate = l_shipdate;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lineitem other)) return false;
        return Objects.equals(this.l_partkey, other.l_partkey) && Objects.equals(this.l_suppkey, other.l_suppkey) && Objects.equals(this.l_quantity, other.l_quantity) && Objects.equals(this.l_shipdate, other.l_shipdate);
    }
    @Override public int hashCode() {
        return Objects.hash(l_partkey, l_suppkey, l_quantity, l_shipdate);
    }
    int size() { return 4; }
}
class Shipped94 {
    int partkey;
    int suppkey;
    Shipped94(int partkey, int suppkey) {
        this.partkey = partkey;
        this.suppkey = suppkey;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Shipped94 other)) return false;
        return Objects.equals(this.partkey, other.partkey) && Objects.equals(this.suppkey, other.suppkey);
    }
    @Override public int hashCode() {
        return Objects.hash(partkey, suppkey);
    }
    int size() { return 2; }
}
class PartkeySuppkeyQty {
    int partkey;
    int suppkey;
    int qty;
    PartkeySuppkeyQty(int partkey, int suppkey, int qty) {
        this.partkey = partkey;
        this.suppkey = suppkey;
        this.qty = qty;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PartkeySuppkeyQty other)) return false;
        return Objects.equals(this.partkey, other.partkey) && Objects.equals(this.suppkey, other.suppkey) && Objects.equals(this.qty, other.qty);
    }
    @Override public int hashCode() {
        return Objects.hash(partkey, suppkey, qty);
    }
    int size() { return 3; }
}
class Result {
    String s_name;
    String s_address;
    Result(String s_name, String s_address) {
        this.s_name = s_name;
        this.s_address = s_address;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Result other)) return false;
        return Objects.equals(this.s_name, other.s_name) && Objects.equals(this.s_address, other.s_address);
    }
    @Override public int hashCode() {
        return Objects.hash(s_name, s_address);
    }
    int size() { return 2; }
}
public class Q20 {
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    static Map<String,Object> asMap(Object o) {
        if (o instanceof Map<?,?> mm) {
            LinkedHashMap<String,Object> m = new LinkedHashMap<>();
            for (Map.Entry<?,?> e : mm.entrySet()) m.put(String.valueOf(e.getKey()), e.getValue());
            return m;
        }
        LinkedHashMap<String,Object> m = new LinkedHashMap<>();
        for (var f : o.getClass().getDeclaredFields()) { try { f.setAccessible(true); m.put(f.getName(), f.get(o)); } catch (Exception e) { throw new RuntimeException(e); } }
        return m;
    }
    static void saveJsonl(List<?> list) {
        for (Object obj : list) {
            Map<String,Object> m = asMap(obj);
            List<String> parts = new ArrayList<>();
            for (Map.Entry<?,?> e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
            System.out.println("{" + String.join(",", parts) + "}");
        }
    }
    static String toJson(Object o) {
        if (o instanceof Map<?,?> m) {
            StringJoiner j = new StringJoiner(",", "{", "}");
            for (Map.Entry<?,?> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + toJson(e.getValue()));
            return j.toString();
        } else if (o instanceof Collection<?> c) {
            StringJoiner j = new StringJoiner(",", "[", "]");
            for (var x : c) j.add(toJson(x));
            return j.toString();
        } else if (o instanceof String s) {
            return "\"" + s + "\"";
        } else if (o instanceof Number || o instanceof Boolean || o instanceof Character) {
            return String.valueOf(o);
        } else {
            Map<String,Object> m = asMap(o);
            StringJoiner j = new StringJoiner(",", "{", "}");
            for (Map.Entry<String,Object> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + toJson(e.getValue()));
            return j.toString();
        }
    }
    static void json(Object o) { System.out.println(toJson(o)); }
    public static void main(String[] args) {
        List<Nation> nation = new ArrayList<>(Arrays.asList(new Nation(1, "CANADA"), new Nation(2, "GERMANY")));
        List<Supplier> supplier = new ArrayList<>(Arrays.asList(new Supplier(100, "Maple Supply", "123 Forest Lane", 1), new Supplier(200, "Berlin Metals", "456 Iron Str", 2)));
        List<Part> part = new ArrayList<>(Arrays.asList(new Part(10, "forest glade bricks"), new Part(20, "desert sand paper")));
        List<Partsupp> partsupp = new ArrayList<>(Arrays.asList(new Partsupp(10, 100, 100), new Partsupp(20, 200, 30)));
        List<Lineitem> lineitem = new ArrayList<>(Arrays.asList(new Lineitem(10, 100, 100, "1994-05-15"), new Lineitem(10, 100, 50, "1995-01-01")));
        String prefix = "forest";
        List<PartkeySuppkeyQty> shipped_94 = (new java.util.function.Supplier<List<PartkeySuppkeyQty>>(){public List<PartkeySuppkeyQty> get(){
    List<PartkeySuppkeyQty> res0 = new ArrayList<>();
    Map<Shipped94,List<Lineitem>> groups1 = new LinkedHashMap<>();
    for (var l : lineitem) {
        if (!(String.valueOf(l.l_shipdate).compareTo(String.valueOf("1994-01-01")) >= 0 && String.valueOf(l.l_shipdate).compareTo(String.valueOf("1995-01-01")) < 0)) continue;
        var row2 = l;
        Shipped94 key3 = new Shipped94(l.l_partkey, l.l_suppkey);
        List<Lineitem> bucket4 = groups1.get(key3);
        if (bucket4 == null) { bucket4 = new ArrayList<>(); groups1.put(key3, bucket4); }
        bucket4.add(row2);
    }
    for (Map.Entry<Shipped94,List<Lineitem>> __e : groups1.entrySet()) {
        Shipped94 g_key = __e.getKey();
        Group<Shipped94,Lineitem> g = new Group<>(g_key, __e.getValue());
        res0.add(new PartkeySuppkeyQty(g.key.partkey, g.key.suppkey, (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> res5 = new ArrayList<>();
    for (var x : g) {
        res5.add(x.l_quantity);
    }
    return res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum()));
    }
    return res0;
}}).get();
        List<Integer> target_partkeys = (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
    List<Integer> res6 = new ArrayList<>();
    for (var ps : partsupp) {
        for (var p : part) {
            if (!(ps.ps_partkey == p.p_partkey)) continue;
            for (var s : shipped_94) {
                if (!(ps.ps_partkey == s.partkey && ps.ps_suppkey == s.suppkey)) continue;
                if (!(Objects.equals(p.p_name.substring(0, prefix.length()), prefix) && ps.ps_availqty > (0.500000 * s.qty))) continue;
                res6.add(ps.ps_suppkey);
            }
        }
    }
    return res6;
}}).get();
        List<Result> result = (new java.util.function.Supplier<List<Result>>(){public List<Result> get(){
    List<Result> res7 = new ArrayList<>();
    for (var s : supplier) {
        for (var n : nation) {
            if (!(n.n_nationkey == s.s_nationkey)) continue;
            if (!(target_partkeys.contains(s.s_suppkey) && Objects.equals(n.n_name, "CANADA"))) continue;
            res7.add(new Result(s.s_name, s.s_address));
        }
    }
    return res7;
}}).get();
        json(result);
        if (!(Objects.equals(result, Arrays.asList(new Result("Maple Supply", "123 Forest Lane"))))) throw new AssertionError("expect failed");
    }
}
