import java.util.*;
public class MapMembership {
    public static void main(String[] args) {
        Map<String, Integer> m = new HashMap<>();
        m.put("a", 1);
        m.put("b", 2);
        System.out.println(m.containsKey("a"));
        System.out.println(m.containsKey("c"));
    }
}
