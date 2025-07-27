public class Main {

    static long mod(long n, long m) {
        return Math.floorMod(((Math.floorMod(n, m)) + m), m);
    }

    static boolean isPrime(long n) {
        if (n < 2L) {
            return false;
        }
        if (Math.floorMod(n, 2L) == 0L) {
            return n == 2L;
        }
        if (Math.floorMod(n, 3L) == 0L) {
            return n == 3L;
        }
        long d = 5L;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0L) {
                return false;
            }
            d = d + 2L;
            if (Math.floorMod(n, d) == 0L) {
                return false;
            }
            d = d + 4L;
        }
        return true;
    }

    static String pad(long n, long width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }

    static void carmichael(long p1) {
        for (long h3 = 2L; h3 < p1; h3++) {
            for (long d = 1L; d < (h3 + p1); d++) {
                if (Math.floorMod(((h3 + p1) * (p1 - 1L)), d) == 0L && mod(-p1 * p1, h3) == Math.floorMod(d, h3)) {
                    long p2 = 1L + ((p1 - 1L) * (h3 + p1) / d);
                    if (!(Boolean)isPrime(p2)) {
                        continue;
                    }
                    long p3 = 1L + (p1 * p2 / h3);
                    if (!(Boolean)isPrime(p3)) {
                        continue;
                    }
                    if (Math.floorMod((p2 * p3), (p1 - 1L)) != 1L) {
                        continue;
                    }
                    long c = p1 * p2 * p3;
                    System.out.println(String.valueOf(pad(p1, 2L)) + "   " + String.valueOf(pad(p2, 4L)) + "   " + String.valueOf(pad(p3, 5L)) + "     " + String.valueOf(c));
                }
            }
        }
    }
    public static void main(String[] args) {
        System.out.println("The following are Carmichael munbers for p1 <= 61:\n");
        System.out.println("p1     p2      p3     product");
        System.out.println("==     ==      ==     =======");
        for (long p1 = 2L; p1 < 62L; p1++) {
            if (isPrime(p1)) {
                carmichael(p1);
            }
        }
    }
}
