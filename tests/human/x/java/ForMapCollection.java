import java.util.*;
public class ForMapCollection {
    public static void main(String[] args) {
        Map<String, Integer> m = new LinkedHashMap<>();
        m.put("a", 1);
        m.put("b", 2);
        for (String k : m.keySet()) {
            System.out.println(k);
        }
    }
}
