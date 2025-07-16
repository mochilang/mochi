// apply-a-callback-to-an-array-2.mochi
import java.util.*;
import java.util.function.*;

public class ApplyACallbackToAnArray2 {
    static void each(List<Integer> xs, Object f) {
        for (Integer x : xs) {
            f(x);
        }
    }
    static List<Integer> Map(List<Integer> xs, IntUnaryOperator f) {
        List<Integer> r = Arrays.asList();
        for (Integer x : xs) {
            r.add(f.applyAsInt(x));
        }
        return r;
    }
    static void main() {
        List<Integer> s = Arrays.asList(1, 2, 3, 4, 5);
        each(s, i -> System.out.println(String.valueOf(i * i)));
        System.out.println(String.valueOf(Map(s, i -> i * i)));
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
