import java.util.*;
public class MapIntKey {
    public static void main(String[] args) {
        Map<Integer, String> m = new HashMap<>();
        m.put(1, "a");
        m.put(2, "b");
        System.out.println(m.get(1));
    }
}
