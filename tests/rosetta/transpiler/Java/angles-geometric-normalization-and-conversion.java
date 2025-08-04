public class Main {

    static double d2d(double d) {
        return d % 360.0;
    }

    static double g2g(double g) {
        return g % 400.0;
    }

    static double m2m(double m) {
        return m % 6400.0;
    }

    static double r2r(double r) {
        return r % (2.0 * 3.141592653589793);
    }

    static double d2g(double d) {
        return d2d(d) * 400.0 / 360.0;
    }

    static double d2m(double d) {
        return d2d(d) * 6400.0 / 360.0;
    }

    static double d2r(double d) {
        return d2d(d) * 3.141592653589793 / 180.0;
    }

    static double g2d(double g) {
        return g2g(g) * 360.0 / 400.0;
    }

    static double g2m(double g) {
        return g2g(g) * 6400.0 / 400.0;
    }

    static double g2r(double g) {
        return g2g(g) * 3.141592653589793 / 200.0;
    }

    static double m2d(double m) {
        return m2m(m) * 360.0 / 6400.0;
    }

    static double m2g(double m) {
        return m2m(m) * 400.0 / 6400.0;
    }

    static double m2r(double m) {
        return m2m(m) * 3.141592653589793 / 3200.0;
    }

    static double r2d(double r) {
        return r2r(r) * 180.0 / 3.141592653589793;
    }

    static double r2g(double r) {
        return r2r(r) * 200.0 / 3.141592653589793;
    }

    static double r2m(double r) {
        return r2r(r) * 3200.0 / 3.141592653589793;
    }

    static void main() {
        double[] angles = ((double[])(new double[]{-2.0, -1.0, 0.0, 1.0, 2.0, 6.2831853, 16.0, 57.2957795, 359.0, 399.0, 6399.0, 1000000.0}));
        System.out.println("degrees normalized_degs gradians mils radians");
        for (double a : angles) {
            System.out.println(_p(a) + " " + _p(d2d(a)) + " " + _p(d2g(a)) + " " + _p(d2m(a)) + " " + _p(d2r(a)));
        }
        System.out.println("\ngradians normalized_grds degrees mils radians");
        for (double a : angles) {
            System.out.println(_p(a) + " " + _p(g2g(a)) + " " + _p(g2d(a)) + " " + _p(g2m(a)) + " " + _p(g2r(a)));
        }
        System.out.println("\nmils normalized_mils degrees gradians radians");
        for (double a : angles) {
            System.out.println(_p(a) + " " + _p(m2m(a)) + " " + _p(m2d(a)) + " " + _p(m2g(a)) + " " + _p(m2r(a)));
        }
        System.out.println("\nradians normalized_rads degrees gradians mils");
        for (double a : angles) {
            System.out.println(_p(a) + " " + _p(r2r(a)) + " " + _p(r2d(a)) + " " + _p(r2g(a)) + " " + _p(r2m(a)));
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
