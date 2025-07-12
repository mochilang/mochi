import java.util.*;

class Inner {
    int inner;
    Inner(int inner) {
        this.inner = inner;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Inner other)) return false;
        return Objects.equals(this.inner, other.inner);
    }
    @Override public int hashCode() {
        return Objects.hash(inner);
    }
    int size() { return 1; }
}
class Outer {
    Inner outer;
    Outer(Inner outer) {
        this.outer = outer;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Outer other)) return false;
        return Objects.equals(this.outer, other.outer);
    }
    @Override public int hashCode() {
        return Objects.hash(outer);
    }
    int size() { return 1; }
}
public class MapNestedAssign {
    static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
    static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
        LinkedHashMap<K,V> m = new LinkedHashMap<>();
        for (var e : entries) m.put(e.getKey(), e.getValue());
        return m;
    }
    public static void main(String[] args) {
    Map<String,Inner> data = mapOfEntries(entry("outer", new Inner(1)));
    ((Map)data.get("outer")).inner = 2;
    System.out.println(((Map)data.get("outer")).inner);
    }
}
