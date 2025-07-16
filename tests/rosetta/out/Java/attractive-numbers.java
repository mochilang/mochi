// attractive-numbers.mochi
import java.util.*;

public class AttractiveNumbers {
    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Objects.equals(n % 2, 0)) {
            return n == 2;
        }
        if (Objects.equals(n % 3, 0)) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Objects.equals(n % d, 0)) {
                return false;
            }
            d = (int)(d + 2);
            if (Objects.equals(n % d, 0)) {
                return false;
            }
            d = (int)(d + 4);
        }
        return true;
    }
    static int countPrimeFactors(int n) {
        if (n == 1) {
            return 0;
        }
        if (isPrime(n)) {
            return 1;
        }
        int count = 0;
        int f = 2;
        while (true) {
            if (Objects.equals(n % f, 0)) {
                count = (int)(count + 1);
                n = (int)(n / f);
                if (n == 1) {
                    return count;
                }
                if (isPrime(n)) {
                    f = (int)(n);
                }
            }
        }
        return count;
    }
    static String pad4(int n) {
        String s = String.valueOf(n);
        while (s.length() < 4) {
            s = " " + s;
        }
        return s;
    }
    static void main() {
        int max = 120;
        System.out.println("The attractive numbers up to and including " + String.valueOf(max) + " are:");
        int count = 0;
        String line = "";
        int lineCount = 0;
        int i = 1;
        while (i <= max) {
            int c = countPrimeFactors(i);
            if (isPrime(c)) {
                line = line + pad4(i);
                count = (int)(count + 1);
                lineCount = (int)(lineCount + 1);
                if (lineCount == 20) {
                    System.out.println(line);
                    line = "";
                    lineCount = (int)(0);
                }
            }
            i = (int)(i + 1);
        }
        if (lineCount > 0) {
            System.out.println(line);
        }
    }
    public static void main(String[] args) {
    main();
    }
}
