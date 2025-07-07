import java.util.*;
public class MapNestedAssign {
    public static void main(String[] args) {
        Map<String, Map<String, Integer>> data = new HashMap<>();
        Map<String, Integer> inner = new HashMap<>();
        inner.put("inner", 1);
        data.put("outer", inner);
        data.get("outer").put("inner", 2);
        System.out.println(data.get("outer").get("inner"));
    }
}
