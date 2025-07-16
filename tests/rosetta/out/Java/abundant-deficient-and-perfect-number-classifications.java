// abundant-deficient-and-perfect-number-classifications.mochi
import java.util.*;

public class AbundantDeficientAndPerfectNumberClassifications {
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
    static void main() {
        int d = 0;
        int a = 0;
        int pnum = 0;
        int i = 1;
        while (i <= 20000) {
            int j = pfacSum(i);
            if (j < i) {
                d = (int)(d + 1);
            }
            if (j == i) {
                pnum = (int)(pnum + 1);
            }
            if (j > i) {
                a = (int)(a + 1);
            }
            i = (int)(i + 1);
        }
        System.out.println("There are " + String.valueOf(d) + " deficient numbers between 1 and 20000");
        System.out.println("There are " + String.valueOf(a) + " abundant numbers  between 1 and 20000");
        System.out.println("There are " + String.valueOf(pnum) + " perfect numbers between 1 and 20000");
    }
    public static void main(String[] args) {
    main();
    }
}
