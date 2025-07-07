import java.util.*;

public class InOperatorExtended {
    public static void main(String[] args) {
        List<Integer> xs = Arrays.asList(1, 2, 3);
        List<Integer> ys = new ArrayList<>();
        for (int x : xs) {
            if (x % 2 == 1) {
                ys.add(x);
            }
        }
        System.out.println(ys.contains(1));
        System.out.println(ys.contains(2));

        Map<String, Integer> m = new HashMap<>();
        m.put("a", 1);
        System.out.println(m.containsKey("a"));
        System.out.println(m.containsKey("b"));

        String s = "hello";
        System.out.println(s.contains("ell"));
        System.out.println(s.contains("foo"));
    }
}
