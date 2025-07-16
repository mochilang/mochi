// associative-array-merging.mochi
import java.util.*;

public class AssociativeArrayMerging {
    static Map<String,any> merge(Map<String,any> base, Map<String,any> update) {
        Map<String,any> result = new LinkedHashMap<String,any>();
        for (String k : base.keySet()) {
            result.put(k, base.get(k));
        }
        for (String k : update.keySet()) {
            result.put(k, update.get(k));
        }
        return result;
    }
    static void main() {
        Map<String,any> base = new NamePriceColor("Rocket Skates", 12.750000, "yellow");
        Map<String,any> update = new PriceColorYear(15.250000, "red", 1974);
        Map<String,any> result = merge(base, update);
        System.out.println(result);
    }
    static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
    static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
        LinkedHashMap<K,V> m = new LinkedHashMap<>();
        for (var e : entries) m.put(e.getKey(), e.getValue());
        return m;
    }
    public static void main(String[] args) {
    main();
    }
}
