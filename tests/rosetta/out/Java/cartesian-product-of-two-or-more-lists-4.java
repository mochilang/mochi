// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
// cartesian-product-of-two-or-more-lists-4.mochi
import java.util.*;

public class CartesianProductOfTwoOrMoreLists4 {
    static String listStr(List<Integer> xs) {
        String s = "[";
        int i = 0;
        while (i < xs.size()) {
            s = s + ((Number)String.valueOf(xs.get(i))).doubleValue();
            if (i < xs.size() - 1) {
                s = s + " ";
            }
            i = (int)(i + 1);
        }
        s = s + "]";
        return s;
    }
    static String llStr(List<List<Integer>> lst) {
        String s = "[";
        int i = 0;
        while (i < lst.size()) {
            s = s + ((Number)listStr(lst.get(i))).doubleValue();
            if (i < lst.size() - 1) {
                s = s + " ";
            }
            i = (int)(i + 1);
        }
        s = s + "]";
        return s;
    }
    static List<Integer> copy(List<Integer> xs) {
        List<Integer> out = Arrays.asList();
        for (Integer v : xs) {
            out.add(v);
        }
        return out;
    }
    static List<List<Integer>> cartN(Object lists) {
        if (Objects.equals(lists, null)) {
            return Arrays.asList();
        }
        List<List<Integer>> a = (List<List<Integer>>)lists;
        if (Objects.equals(a.size(), 0)) {
            return Arrays.asList(Arrays.asList());
        }
        List<List<Integer>> out = Arrays.asList();
        int last = a.size() - 1;
        List<List<Integer>> left = cartN(((List)a).subList(0, last));
        for (List<Integer> p : left) {
            for (List<Integer> x : (List<List<Integer>>)a.get(last)) {
                List<Integer> row = copy(p);
                row.add(x);
                out.add(row);
            }
        }
        return out;
    }
    static void main() {
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(1, 2), Arrays.asList(3, 4)))));
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(3, 4), Arrays.asList(1, 2)))));
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(1, 2), Arrays.asList()))));
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(), Arrays.asList(1, 2)))));
        System.out.println("");
        System.out.println("[");
        for (List<Integer> p : cartN(Arrays.asList(Arrays.asList(1776, 1789), Arrays.asList(7, 12), Arrays.asList(4, 14, 23), Arrays.asList(0, 1)))) {
            System.out.println(" " + listStr(p));
        }
        System.out.println("]");
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(1, 2, 3), Arrays.asList(30), Arrays.asList(500, 100)))));
        System.out.println(llStr(cartN(Arrays.asList(Arrays.asList(1, 2, 3), Arrays.asList(), Arrays.asList(500, 100)))));
        System.out.println("");
        System.out.println(llStr(cartN(null)));
        System.out.println(llStr(cartN(Arrays.asList())));
    }
    public static void main(String[] args) {
        main();
    }
}
