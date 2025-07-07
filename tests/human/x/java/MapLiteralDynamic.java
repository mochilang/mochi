import java.util.*;
public class MapLiteralDynamic {
    public static void main(String[] args) {
        int x = 3;
        int y = 4;
        Map<String, Integer> m = new HashMap<>();
        m.put("a", x);
        m.put("b", y);
        System.out.println(m.get("a") + " " + m.get("b"));
    }
}
