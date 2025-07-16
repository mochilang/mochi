// cartesian-product-of-two-or-more-lists-1.mochi
import java.util.*;

public class CartesianProductOfTwoOrMoreLists1 {
    static List<List<Integer>> cart2(List<Integer> a, List<Integer> b) {
        List<List<Integer>> p = Arrays.asList();
        for (Integer x : a) {
            for (Integer y : b) {
                p.add(Arrays.asList(x, y));
            }
        }
        return p;
    }
    static String llStr(List<List<Integer>> lst) {
        String s = "[";
        int i = 0;
        while (i < lst.size()) {
            List<List<Integer>> row = lst.get(i);
            s = s + "[";
            int j = 0;
            while (j < row.size()) {
                s = s + ((Number)String.valueOf(row.get(j))).doubleValue();
                if (j < row.size() - 1) {
                    s = s + " ";
                }
                j = (int)(j + 1);
            }
            s = s + "]";
            if (i < lst.size() - 1) {
                s = s + " ";
            }
            i = (int)(i + 1);
        }
        s = s + "]";
        return s;
    }
    static void main() {
        System.out.println(llStr(cart2(Arrays.asList(1, 2), Arrays.asList(3, 4))));
        System.out.println(llStr(cart2(Arrays.asList(3, 4), Arrays.asList(1, 2))));
        System.out.println(llStr(cart2(Arrays.asList(1, 2), Arrays.asList())));
        System.out.println(llStr(cart2(Arrays.asList(), Arrays.asList(1, 2))));
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
