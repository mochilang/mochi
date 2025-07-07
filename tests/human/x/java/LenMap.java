import java.util.*;
public class LenMap {
    public static void main(String[] args) {
        Map<String, Integer> map = new HashMap<>();
        map.put("a", 1);
        map.put("b", 2);
        int len = map.size();
        System.out.println(len);
    }
}
