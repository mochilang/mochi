public class Main {
    static int qlimit;

    static double powf(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double g = x;
        int i_1 = 0;
        while (i_1 < 20) {
            g = (g + x / g) / 2.0;
            i_1 = i_1 + 1;
        }
        return g;
    }

    static int modPow(int base, int exp, int mod) {
        int result_1 = Math.floorMod(1, mod);
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result_1 = Math.floorMod((result_1 * b), mod);
            }
            b = Math.floorMod((b * b), mod);
            e = e / 2;
        }
        return result_1;
    }

    static void mtest(int m) {
        if (m < 4) {
            System.out.println((String)(_p(m)) + " < 4.  M" + (String)(_p(m)) + " not tested.");
            return;
        }
        double flimit = sqrtApprox(powf(2.0, m) - 1.0);
        int qlast = 0;
        if (flimit < qlimit) {
            qlast = ((Number)(flimit)).intValue();
        } else {
            qlast = qlimit;
        }
        boolean[] composite = new boolean[]{};
        int i_2 = 0;
        while (i_2 <= qlast) {
            composite = appendBool(composite, false);
            i_2 = i_2 + 1;
        }
        int sq = ((Number)(sqrtApprox(((Number)(qlast)).doubleValue()))).intValue();
        int q = 3;
        while (true) {
            if (q <= sq) {
                int j = q * q;
                while (j <= qlast) {
composite[j] = true;
                    j = j + q;
                }
            }
            int q8 = Math.floorMod(q, 8);
            if ((q8 == 1 || q8 == 7) && modPow(2, m, q) == 1) {
                System.out.println("M" + (String)(_p(m)) + " has factor " + (String)(_p(q)));
                return;
            }
            while (true) {
                q = q + 2;
                if (q > qlast) {
                    System.out.println("No factors of M" + (String)(_p(m)) + " found.");
                    return;
                }
                if (!(Boolean)composite[q]) {
                    break;
                }
            }
        }
    }

    static void main() {
        mtest(31);
        mtest(67);
    }
    public static void main(String[] args) {
        qlimit = 50000;
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
