import java.util.*;

class Alice {
    int alice;
    Alice(int alice) {
        this.alice = alice;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Alice other)) return false;
        return Objects.equals(this.alice, other.alice);
    }
    @Override public int hashCode() {
        return Objects.hash(alice);
    }
    int size() { return 1; }
}
public class MapAssign {
    static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
    static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
        LinkedHashMap<K,V> m = new LinkedHashMap<>();
        for (var e : entries) m.put(e.getKey(), e.getValue());
        return m;
    }
    public static void main(String[] args) {
    Map<String,Integer> scores = mapOfEntries(entry("alice", 1));
    scores.put("bob", 2);
    System.out.println(scores.get("bob"));
    }
}
