public class Main {

    static int mod(int n, int m) {
        return Math.floorMod(((Math.floorMod(n, m)) + m), m);
    }

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
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
                if (Math.floorMod(((h3 + p1) * (p1 - 1)), d) == 0 && mod(-p1 * p1, h3) == Math.floorMod(d, h3)) {
                    int p2 = 1 + ((p1 - 1) * (h3 + p1) / d);
                    if (!(Boolean)isPrime(p2)) {
                        continue;
                    }
                    int p3 = 1 + (p1 * p2 / h3);
                    if (!(Boolean)isPrime(p3)) {
                        continue;
                    }
                    if (Math.floorMod((p2 * p3), (p1 - 1)) != 1) {
                        continue;
                    }
                    int c = p1 * p2 * p3;
                    System.out.println(String.valueOf(pad(p1, 2)) + "   " + String.valueOf(pad(p2, 4)) + "   " + String.valueOf(pad(p3, 5)) + "     " + String.valueOf(c));
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
