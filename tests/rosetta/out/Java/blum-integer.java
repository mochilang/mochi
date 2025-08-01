// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
// blum-integer.mochi
import java.util.*;

public class BlumInteger {
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
    static int firstPrimeFactor(int n) {
        if (n == 1) {
            return 1;
        }
        if (Objects.equals(n % 3, 0)) {
            return 3;
        }
        if (Objects.equals(n % 5, 0)) {
            return 5;
        }
        List<Integer> inc = new ArrayList<>(Arrays.asList(4, 2, 4, 2, 4, 6, 2, 6));
        int k = 7;
        int i = 0;
        while (k * k <= n) {
            if (Objects.equals(n % k, 0)) {
                return k;
            }
            k = (int)(k + ((Number)inc.get(i)).doubleValue());
            i = (int)((i + 1) % inc.size());
        }
        return n;
    }
    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if (Objects.equals(s.substring(i, i + 1), ch)) {
                return i;
            }
            i = (int)(i + 1);
        }
        return -1;
    }
    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }
    static String formatFloat(double f, int prec) {
        String s = String.valueOf(f);
        int idx = indexOf(s, ".");
        if (idx < 0) {
            return s;
        }
        int need = idx + 1 + prec;
        if (s.length() > need) {
            return s.substring(0, need);
        }
        return s;
    }
    static void main() {
        List<Integer> blum = Arrays.asList();
        List<Integer> counts = new ArrayList<>(Arrays.asList(0, 0, 0, 0));
        List<Integer> digits = new ArrayList<>(Arrays.asList(1, 3, 7, 9));
        int i = 1;
        int bc = 0;
        while (true) {
            int p = firstPrimeFactor(i);
            if (Objects.equals(p % 4, 3)) {
                int q = Integer.parseInt((i / p));
                if (q != p && Objects.equals(q % 4, 3) && isPrime(q)) {
                    if (bc < 50) {
                        blum.add(i);
                    }
                    int d = i % 10;
                    if (d == 1) {
                        counts.set(0, ((Number)counts.get(0)).doubleValue() + 1);
                    }
                    bc = (int)(bc + 1);
                    if (bc == 50) {
                        System.out.println("First 50 Blum integers:");
                        int idx = 0;
                        while (idx < 50) {
                            String line = "";
                            int j = 0;
                            while (j < 10) {
                                line = ((Number)line + ((Number)padLeft(blum.get(idx), 3)).doubleValue()).doubleValue() + " ";
                                idx = (int)(idx + 1);
                                j = (int)(j + 1);
                            }
                            System.out.println(line.substring(0, line.length() - 1));
                        }
                        break;
                    }
                }
            }
            if (Objects.equals(i % 5, 3)) {
                i = (int)(i + 4);
            }
            else {
                i = (int)(i + 2);
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
