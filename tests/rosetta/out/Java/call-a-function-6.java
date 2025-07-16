// call-a-function-6.mochi
import java.util.*;

public class CallAFunction6 {
    static void bar(int a, int b, int c) {
        System.out.println(String.valueOf(a) + ", " + String.valueOf(b) + ", " + String.valueOf(c));
    }
    static void main() {
        Map<String,Integer> args = new LinkedHashMap<String,Integer>();
        args.put("a", 3);
        args.put("b", 2);
        args.put("c", 1);
        bar(args.get("a"), args.get("b"), args.get("c"));
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
