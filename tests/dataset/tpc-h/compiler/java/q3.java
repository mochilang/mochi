// Generated by Mochi compiler v0.10.28 on 2025-07-18T06:59:44Z
// q3.mochi
import java.util.*;

class Customer {
    int c_custkey;
    String c_mktsegment;
    Customer(int c_custkey, String c_mktsegment) {
        this.c_custkey = c_custkey;
        this.c_mktsegment = c_mktsegment;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Customer other)) return false;
        return Objects.equals(this.c_custkey, other.c_custkey) && Objects.equals(this.c_mktsegment, other.c_mktsegment);
    }
    @Override public int hashCode() {
        return Objects.hash(c_custkey, c_mktsegment);
    }
    int size() { return 2; }
}
class Order {
    int o_orderkey;
    int o_custkey;
    String o_orderdate;
    int o_shippriority;
    Order(int o_orderkey, int o_custkey, String o_orderdate, int o_shippriority) {
        this.o_orderkey = o_orderkey;
        this.o_custkey = o_custkey;
        this.o_orderdate = o_orderdate;
        this.o_shippriority = o_shippriority;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Order other)) return false;
        return Objects.equals(this.o_orderkey, other.o_orderkey) && Objects.equals(this.o_custkey, other.o_custkey) && Objects.equals(this.o_orderdate, other.o_orderdate) && Objects.equals(this.o_shippriority, other.o_shippriority);
    }
    @Override public int hashCode() {
        return Objects.hash(o_orderkey, o_custkey, o_orderdate, o_shippriority);
    }
    int size() { return 4; }
}
class Lineitem {
    int l_orderkey;
    double l_extendedprice;
    double l_discount;
    String l_shipdate;
    Lineitem(int l_orderkey, double l_extendedprice, double l_discount, String l_shipdate) {
        this.l_orderkey = l_orderkey;
        this.l_extendedprice = l_extendedprice;
        this.l_discount = l_discount;
        this.l_shipdate = l_shipdate;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lineitem other)) return false;
        return Objects.equals(this.l_orderkey, other.l_orderkey) && Objects.equals(this.l_extendedprice, other.l_extendedprice) && Objects.equals(this.l_discount, other.l_discount) && Objects.equals(this.l_shipdate, other.l_shipdate);
    }
    @Override public int hashCode() {
        return Objects.hash(l_orderkey, l_extendedprice, l_discount, l_shipdate);
    }
    int size() { return 4; }
}
class OrderLineJoin {
    Order o;
    Lineitem l;
    OrderLineJoin(Order o, Lineitem l) {
        this.o = o;
        this.l = l;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderLineJoin other)) return false;
        return Objects.equals(this.o, other.o) && Objects.equals(this.l, other.l);
    }
    @Override public int hashCode() {
        return Objects.hash(o, l);
    }
    int size() { return 2; }
}
class OOrderkeyOOrderdateOShippriority {
    int o_orderkey;
    String o_orderdate;
    int o_shippriority;
    OOrderkeyOOrderdateOShippriority(int o_orderkey, String o_orderdate, int o_shippriority) {
        this.o_orderkey = o_orderkey;
        this.o_orderdate = o_orderdate;
        this.o_shippriority = o_shippriority;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OOrderkeyOOrderdateOShippriority other)) return false;
        return Objects.equals(this.o_orderkey, other.o_orderkey) && Objects.equals(this.o_orderdate, other.o_orderdate) && Objects.equals(this.o_shippriority, other.o_shippriority);
    }
    @Override public int hashCode() {
        return Objects.hash(o_orderkey, o_orderdate, o_shippriority);
    }
    int size() { return 3; }
}
class LOrderkeyRevenueOOrderdateOShippriority {
    int l_orderkey;
    double revenue;
    String o_orderdate;
    int o_shippriority;
    LOrderkeyRevenueOOrderdateOShippriority(int l_orderkey, double revenue, String o_orderdate, int o_shippriority) {
        this.l_orderkey = l_orderkey;
        this.revenue = revenue;
        this.o_orderdate = o_orderdate;
        this.o_shippriority = o_shippriority;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LOrderkeyRevenueOOrderdateOShippriority other)) return false;
        return Objects.equals(this.l_orderkey, other.l_orderkey) && Objects.equals(this.revenue, other.revenue) && Objects.equals(this.o_orderdate, other.o_orderdate) && Objects.equals(this.o_shippriority, other.o_shippriority);
    }
    @Override public int hashCode() {
        return Objects.hash(l_orderkey, revenue, o_orderdate, o_shippriority);
    }
    int size() { return 4; }
}
public class Q3 {
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
        List<Customer> customer = new ArrayList<>(Arrays.asList(new Customer(1, "BUILDING"), new Customer(2, "AUTOMOBILE")));
        List<Order> orders = new ArrayList<>(Arrays.asList(new Order(100, 1, "1995-03-14", 1), new Order(200, 2, "1995-03-10", 2)));
        List<Lineitem> lineitem = new ArrayList<>(Arrays.asList(new Lineitem(100, 1000.000000, 0.050000, "1995-03-16"), new Lineitem(100, 500.000000, 0.000000, "1995-03-20"), new Lineitem(200, 1000.000000, 0.100000, "1995-03-14")));
        String cutoff = "1995-03-15";
        String segment = "BUILDING";
        List<Customer> building_customers = (new java.util.function.Supplier<List<Customer>>(){public List<Customer> get(){
    List<Customer> res0 = new ArrayList<>();
    for (var c : customer) {
        if (!(Objects.equals(c.c_mktsegment, segment))) continue;
        res0.add(c);
    }
    return res0;
}}).get();
        List<Order> valid_orders = (new java.util.function.Supplier<List<Order>>(){public List<Order> get(){
    List<Order> res1 = new ArrayList<>();
    for (var o : orders) {
        for (var c : building_customers) {
            if (!(o.o_custkey == c.c_custkey)) continue;
            if (!(String.valueOf(o.o_orderdate).compareTo(String.valueOf(cutoff)) < 0)) continue;
            res1.add(o);
        }
    }
    return res1;
}}).get();
        List<Lineitem> valid_lineitems = (new java.util.function.Supplier<List<Lineitem>>(){public List<Lineitem> get(){
    List<Lineitem> res2 = new ArrayList<>();
    for (var l : lineitem) {
        if (!(String.valueOf(l.l_shipdate).compareTo(String.valueOf(cutoff)) > 0)) continue;
        res2.add(l);
    }
    return res2;
}}).get();
        List<LOrderkeyRevenueOOrderdateOShippriority> order_line_join = (new java.util.function.Supplier<List<LOrderkeyRevenueOOrderdateOShippriority>>(){public List<LOrderkeyRevenueOOrderdateOShippriority> get(){
    List<LOrderkeyRevenueOOrderdateOShippriority> res3 = new ArrayList<>();
    Map<OOrderkeyOOrderdateOShippriority,List<OrderLineJoin>> groups4 = new LinkedHashMap<>();
    for (var o : valid_orders) {
        for (var l : valid_lineitems) {
            if (!(l.l_orderkey == o.o_orderkey)) continue;
            OrderLineJoin row5 = new OrderLineJoin(o, l);
            OOrderkeyOOrderdateOShippriority key6 = new OOrderkeyOOrderdateOShippriority(o.o_orderkey, o.o_orderdate, o.o_shippriority);
            List<OrderLineJoin> bucket7 = groups4.get(key6);
            if (bucket7 == null) { bucket7 = new ArrayList<>(); groups4.put(key6, bucket7); }
            bucket7.add(row5);
        }
    }
    for (Map.Entry<OOrderkeyOOrderdateOShippriority,List<OrderLineJoin>> __e : groups4.entrySet()) {
        OOrderkeyOOrderdateOShippriority g_key = __e.getKey();
        Group<OOrderkeyOOrderdateOShippriority,OrderLineJoin> g = new Group<>(g_key, __e.getValue());
        res3.add(new LOrderkeyRevenueOOrderdateOShippriority(g.key.o_orderkey, (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> res8 = new ArrayList<>();
    for (var r : g) {
        res8.add(r.l.l_extendedprice * (1 - r.l.l_discount));
    }
    return res8;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).sum(), g.key.o_orderdate, g.key.o_shippriority));
    }
    return res3;
}}).get();
        json(order_line_join);
        if (!(Objects.equals(order_line_join, Arrays.asList(new LOrderkeyRevenueOOrderdateOShippriority(100, 1000.000000 * 0.950000 + 500.000000, "1995-03-14", 1))))) throw new AssertionError("expect failed");
    }
}
