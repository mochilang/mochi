// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// anonymous-recursion-1.mochi
import java.util.*;

public class AnonymousRecursion1 {
    static int fib(int n) {
        if (n < 2) {
            return n;
        }
        int a = 0;
        int b = 1;
        int i = 1;
        while (i < n) {
            int t = a + b;
            a = (int)(b);
            b = (int)(t);
            i = (int)(i + 1);
        }
        return b;
    }
    static void main() {
        for (Integer n : Arrays.asList(0, 1, 2, 3, 4, 5, 10, 40, -1)) {
            if (n < 0) {
                System.out.println("fib undefined for negative numbers");
            }
            else {
                System.out.println("fib " + String.valueOf(n) + " = " + String.valueOf(fib(n)));
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
