public class Main {

    static void printExpI(int b, int p) {
        if (p < 0) {
            System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": negative power not allowed");
            return;
        }
        int r = 1;
        int i = 1;
        while (i <= p) {
            r = r * b;
            i = i + 1;
        }
        System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": " + (String)(_p(r)));
    }

    static double expF(double b, int p) {
        boolean neg = false;
        if (p < 0) {
            neg = true;
            p = -p;
        }
        double r_1 = 1.0;
        double pow = b;
        while (p > 0) {
            if (Math.floorMod(p, 2) == 1) {
                r_1 = r_1 * pow;
            }
            pow = pow * pow;
            p = p / 2;
        }
        if (neg) {
            r_1 = 1.0 / r_1;
        }
        return r_1;
    }

    static void printExpF(double b, int p) {
        if (b == 0.0 && p < 0) {
            System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": +Inf");
            return;
        }
        System.out.println((String)(_p(b)) + "^" + (String)(_p(p)) + ": " + (String)(_p(expF(b, p))));
    }
    public static void main(String[] args) {
        System.out.println("expI tests");
        printExpI(2, 10);
        printExpI(2, -10);
        printExpI(-2, 10);
        printExpI(-2, 11);
        printExpI(11, 0);
        System.out.println("overflow undetected");
        printExpI(10, 10);
        System.out.println("\nexpF tests:");
        printExpF(2.0, 10);
        printExpF(2.0, -10);
        printExpF(-2.0, 10);
        printExpF(-2.0, 11);
        printExpF(11.0, 0);
        System.out.println("disallowed in expI, allowed here");
        printExpF(0.0, -1);
        System.out.println("other interesting cases for 32 bit float type");
        printExpF(10.0, 39);
        printExpF(10.0, -39);
        printExpF(-10.0, 39);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
