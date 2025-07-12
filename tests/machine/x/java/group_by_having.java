// group_by_having.mochi
import java.util.*;

class NameCity {
    String name;
    String city;
    NameCity(String name, String city) {
        this.name = name;
        this.city = city;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NameCity other)) return false;
        return Objects.equals(this.name, other.name) && Objects.equals(this.city, other.city);
    }
    @Override public int hashCode() {
        return Objects.hash(name, city);
    }
    int size() { return 2; }
}
class CityNum {
    Object city;
    int num;
    CityNum(Object city, int num) {
        this.city = city;
        this.num = num;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CityNum other)) return false;
        return Objects.equals(this.city, other.city) && Objects.equals(this.num, other.num);
    }
    @Override public int hashCode() {
        return Objects.hash(city, num);
    }
    int size() { return 2; }
}
public class GroupByHaving {
    static class Group<K,V> implements Iterable<V> {
        K key;
        List<V> items;
        Group(K key, List<V> items) { this.key = key; this.items = items; }
        public Iterator<V> iterator() { return items.iterator(); }
        int size() { return items.size(); }
    }
    static String toJson(Object o) {
        if (o instanceof Map<?,?> m) {
            StringJoiner j = new StringJoiner(",", "{", "}");
            for (Map.Entry<?,?> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + e.getValue());
            return j.toString();
        } else if (o instanceof Collection<?> c) {
            StringJoiner j = new StringJoiner(",", "[", "]");
            for (var x : c) j.add(toJson(x));
            return j.toString();
        } else if (o instanceof String s) {
            return "\"" + s + "\"";
        }
        return String.valueOf(o);
    }
    static void json(Object o) { System.out.println(toJson(o)); }
    public static void main(String[] args) {
    List<NameCity> people = new ArrayList<>(Arrays.asList(new NameCity("Alice", "Paris"), new NameCity("Bob", "Hanoi"), new NameCity("Charlie", "Paris"), new NameCity("Diana", "Hanoi"), new NameCity("Eve", "Paris"), new NameCity("Frank", "Hanoi"), new NameCity("George", "Paris")));
    List<CityNum> big = (new java.util.function.Supplier<List<CityNum>>(){public List<CityNum> get(){
    List<CityNum> res0 = new ArrayList<>();
    Map<String,List<NameCity>> groups1 = new LinkedHashMap<>();
    for (var p : people) {
        var row2 = p;
        String key3 = p.city;
        List<NameCity> bucket4 = groups1.get(key3);
        if (bucket4 == null) { bucket4 = new ArrayList<>(); groups1.put(key3, bucket4); }
        bucket4.add(row2);
    }
    for (Map.Entry<String,List<NameCity>> __e : groups1.entrySet()) {
        String g_key = __e.getKey();
        Group<String,NameCity> g = new Group<>(g_key, __e.getValue());
        if (!(g.size() >= 4)) continue;
        res0.add(new CityNum(g.key, g.size()));
    }
    return res0;
}}).get();
    json(big);
    }
}
