// amicable-pairs.mochi
import java.util.*;

public class AmicablePairs {
    static int pfacSum(int i) {
        int sum = 0;
        int p = 1;
        while (p <= i / 2) {
            if (Objects.equals(i % p, 0)) {
                sum = (int)(sum + p);
            }
            p = (int)(p + 1);
        }
        return sum;
    }
    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }
    static void main() {
        List<Integer> sums = Arrays.asList();
        int i = 0;
        while (i < 20000) {
            sums.add(0);
            i = (int)(i + 1);
        }
        i = (int)(1);
        while (i < 20000) {
            sums.set(i, pfacSum(i));
            i = (int)(i + 1);
        }
        System.out.println("The amicable pairs below 20,000 are:");
        int n = 2;
        while (n < 19999) {
            List<Integer> m = sums.get(n);
            if (m > n && m < 20000 && Objects.equals(n, sums.get(m))) {
                System.out.println("  " + pad(n, 5) + " and " + pad(m, 5));
            }
            n = (int)(n + 1);
        }
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
