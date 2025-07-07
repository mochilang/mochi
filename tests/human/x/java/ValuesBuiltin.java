import java.util.*;
public class ValuesBuiltin {
    public static void main(String[] args) {
        Map<String, Integer> m = new LinkedHashMap<>();
        m.put("a", 1);
        m.put("b", 2);
        m.put("c", 3);
        System.out.println(new ArrayList<>(m.values()));
    }
}
