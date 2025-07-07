import java.util.*;
public class MapInOperator {
    public static void main(String[] args) {
        Map<Integer, String> m = new HashMap<>();
        m.put(1, "a");
        m.put(2, "b");
        System.out.println(m.containsKey(1));
        System.out.println(m.containsKey(3));
    }
}
