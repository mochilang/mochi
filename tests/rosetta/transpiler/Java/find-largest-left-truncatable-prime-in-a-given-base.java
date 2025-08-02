public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        int i = 2;
        while (i * i <= n) {
            if (Math.floorMod(n, i) == 0) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int search(int base, int prefix, int depth, int limit, int best) {
        int b = best;
        int d = 1;
        while (d < base) {
            int val = prefix * base + d;
            if (((Boolean)(isPrime(val)))) {
                if (val > b) {
                    b = val;
                }
                if (depth + 1 < limit) {
                    b = search(base, val, depth + 1, limit, b);
                }
            }
            d = d + 1;
        }
        return b;
    }

    static int largest(int base) {
        return search(base, 0, 0, 6, 0);
    }

    static void main() {
        int b_1 = 3;
        while (b_1 <= 17) {
            System.out.println(_p(b_1) + ": " + _p(largest(b_1)));
            b_1 = b_1 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
