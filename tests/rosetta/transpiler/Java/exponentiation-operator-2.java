public class Main {

    static int expI(int b, int p) {
        int r = 1;
        int i = 0;
        while (i < p) {
            r = r * b;
            i = i + 1;
        }
        return r;
    }

    static double expF(double b, int p) {
        double r_1 = 1.0;
        double pow = b;
        int n = p;
        boolean neg = false;
        if (p < 0) {
            n = -p;
            neg = true;
        }
        while (n > 0) {
            if (Math.floorMod(n, 2) == 1) {
                r_1 = r_1 * pow;
            }
            pow = pow * pow;
            n = n / 2;
        }
        if (neg) {
            r_1 = 1.0 / r_1;
        }
        return r_1;
    }

    static void printExpF(double b, int p) {
        if (b == 0.0 && p < 0) {
            System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": +Inf");
        } else {
            System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": " + (String)(_p(expF(b, p))));
        }
    }

    static void main() {
        System.out.println("expI tests");
        for (int[] pair : new int[][]{new int[]{2, 10}, new int[]{2, -10}, new int[]{-2, 10}, new int[]{-2, 11}, new int[]{11, 0}}) {
            if (pair[1] < 0) {
                System.out.println((String)(_p(_geti(pair, 0))) + "^" + (String)(_p(_geti(pair, 1))) + ": negative power not allowed");
            } else {
                System.out.println((String)(_p(_geti(pair, 0))) + "^" + (String)(_p(_geti(pair, 1))) + ": " + (String)(_p(expI(pair[0], pair[1]))));
            }
        }
        System.out.println("overflow undetected");
        System.out.println("10^10: " + (String)(_p(expI(10, 10))));
        System.out.println("\nexpF tests:");
        for (Object[] pair : new Object[][]{new Object[]{2.0, 10}, new Object[]{2.0, -10}, new Object[]{-2.0, 10}, new Object[]{-2.0, 11}, new Object[]{11.0, 0}}) {
            printExpF(((Number)(pair[0])).doubleValue(), ((Number)(pair[1])).intValue());
        }
        System.out.println("disallowed in expI, allowed here");
        printExpF(0.0, -1);
        System.out.println("other interesting cases for 32 bit float type");
        printExpF(10.0, 39);
        printExpF(10.0, -39);
        printExpF(-10.0, 39);
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
