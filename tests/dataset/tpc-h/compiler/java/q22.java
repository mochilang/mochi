// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:00:01Z
// q22.mochi
import java.util.*;

class Customer {
    int c_custkey;
    String c_phone;
    double c_acctbal;
    Customer(int c_custkey, String c_phone, double c_acctbal) {
        this.c_custkey = c_custkey;
        this.c_phone = c_phone;
        this.c_acctbal = c_acctbal;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Customer other)) return false;
        return Objects.equals(this.c_custkey, other.c_custkey) && Objects.equals(this.c_phone, other.c_phone) && Objects.equals(this.c_acctbal, other.c_acctbal);
    }
    @Override public int hashCode() {
        return Objects.hash(c_custkey, c_phone, c_acctbal);
    }
    int size() { return 3; }
}
class Order {
    int o_orderkey;
    int o_custkey;
    Order(int o_orderkey, int o_custkey) {
        this.o_orderkey = o_orderkey;
        this.o_custkey = o_custkey;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Order other)) return false;
        return Objects.equals(this.o_orderkey, other.o_orderkey) && Objects.equals(this.o_custkey, other.o_custkey);
    }
    @Override public int hashCode() {
        return Objects.hash(o_orderkey, o_custkey);
    }
    int size() { return 2; }
}
class EligibleCustomer {
    String cntrycode;
    double c_acctbal;
    EligibleCustomer(String cntrycode, double c_acctbal) {
        this.cntrycode = cntrycode;
        this.c_acctbal = c_acctbal;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof EligibleCustomer other)) return false;
        return Objects.equals(this.cntrycode, other.cntrycode) && Objects.equals(this.c_acctbal, other.c_acctbal);
    }
    @Override public int hashCode() {
        return Objects.hash(cntrycode, c_acctbal);
    }
    int size() { return 2; }
}
class Row {
    String cntrycode;
    int numcust;
    double totacctbal;
    Row(String cntrycode, int numcust, double totacctbal) {
        this.cntrycode = cntrycode;
        this.numcust = numcust;
        this.totacctbal = totacctbal;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Row other)) return false;
        return Objects.equals(this.cntrycode, other.cntrycode) && Objects.equals(this.numcust, other.numcust) && Objects.equals(this.totacctbal, other.totacctbal);
    }
    @Override public int hashCode() {
        return Objects.hash(cntrycode, numcust, totacctbal);
    }
    int size() { return 3; }
}
public class Q22 {
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
        List<Customer> customer = new ArrayList<>(Arrays.asList(new Customer(1, "13-123-4567", 600.000000), new Customer(2, "31-456-7890", 100.000000), new Customer(3, "30-000-0000", 700.000000)));
        List<Order> orders = new ArrayList<>(Arrays.asList(new Order(10, 2)));
        List<String> valid_codes = new ArrayList<>(Arrays.asList("13", "31", "23", "29", "30", "18", "17"));
        double avg_balance = (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> res0 = new ArrayList<>();
    for (var c : customer) {
        if (!(c.c_acctbal > 0.000000 && valid_codes.contains(c.c_phone.substring(0, 2)))) continue;
        res0.add(c.c_acctbal);
    }
    return res0;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).average().orElse(0);
        List<EligibleCustomer> eligible_customers = (new java.util.function.Supplier<List<EligibleCustomer>>(){public List<EligibleCustomer> get(){
    List<EligibleCustomer> res1 = new ArrayList<>();
    for (var c : customer) {
        if (!(valid_codes.contains(c.c_phone.substring(0, 2)) && c.c_acctbal > avg_balance && (!((Boolean)orders.stream().anyMatch(o -> Objects.equals(o.o_custkey, c.c_custkey)))))) continue;
        res1.add(new EligibleCustomer(c.c_phone.substring(0, 2), c.c_acctbal));
    }
    return res1;
}}).get();
        List<Group<String,EligibleCustomer>> groups = (new java.util.function.Supplier<List<Group<String,EligibleCustomer>>>(){public List<Group<String,EligibleCustomer>> get(){
    List<Group<String,EligibleCustomer>> res2 = new ArrayList<>();
    Map<String,List<EligibleCustomer>> groups3 = new LinkedHashMap<>();
    for (var c : eligible_customers) {
        var row4 = c;
        String key5 = c.cntrycode;
        List<EligibleCustomer> bucket6 = groups3.get(key5);
        if (bucket6 == null) { bucket6 = new ArrayList<>(); groups3.put(key5, bucket6); }
        bucket6.add(row4);
    }
    for (Map.Entry<String,List<EligibleCustomer>> __e : groups3.entrySet()) {
        String g_key = __e.getKey();
        Group<String,EligibleCustomer> g = new Group<>(g_key, __e.getValue());
        res2.add(g);
    }
    return res2;
}}).get();
        List<Object> tmp = new ArrayList<>(Arrays.asList());
        for (Group<String,EligibleCustomer> g : groups) {
            double total = (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> res7 = new ArrayList<>();
    for (var x : g.items) {
        res7.add(x.c_acctbal);
    }
    return res7;
}}).get().stream().mapToDouble(n -> ((Number)n).doubleValue()).sum();
            Row row = new Row(g.key, g.size(), total);
            tmp.add(row);
        }
        List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
    List<Object> res8 = new ArrayList<>();
    for (var r : tmp) {
        res8.add(r);
    }
    return res8;
}}).get();
        json(result);
        if (!(Objects.equals(result, Arrays.asList(new Row("13", 1, 600.000000), new Row("30", 1, 700.000000))))) throw new AssertionError("expect failed");
    }
}
