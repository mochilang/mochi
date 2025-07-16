// carmichael-3-strong-pseudoprimes.mochi
import java.util.*;

public class Carmichael3StrongPseudoprimes {
    static int mod(int n, int m) {
        return ((n % m) + m) % m;
    }
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
    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }
    static void carmichael(int p1) {
        for (int h3 = 2; h3 < p1; h3++) {
            for (int d = 1; d < (h3 + p1); d++) {
                if (Objects.equals(((h3 + p1) * (p1 - 1)) % d, 0) && Objects.equals(mod(-p1 * p1, h3), d % h3)) {
                    int p2 = 1 + ((p1 - 1) * (h3 + p1) / d);
                    if (!isPrime(p2)) {
                        continue;
                    }
                    int p3 = 1 + (p1 * p2 / h3);
                    if (!isPrime(p3)) {
                        continue;
                    }
                    if (!Objects.equals((p2 * p3) % (p1 - 1), 1)) {
                        continue;
                    }
                    int c = p1 * p2 * p3;
                    System.out.println(pad(p1, 2) + "   " + pad(p2, 4) + "   " + pad(p3, 5) + "     " + String.valueOf(c));
                }
            }
        }
    }
    public static void main(String[] args) {
    System.out.println("The following are Carmichael munbers for p1 <= 61:\n");
    System.out.println("p1     p2      p3     product");
    System.out.println("==     ==      ==     =======");
    for (int p1 = 2; p1 < 62; p1++) {
        if (isPrime(p1)) {
            carmichael(p1);
        }
    }
    }
}
