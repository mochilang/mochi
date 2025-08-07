public class Main {
    static double PI;
    static double TWO_PI;
    static double[][] forces1;
    static double[][] location1;
    static double[][] forces2;
    static double[][] location2;
    static double[][] forces3;
    static double[][] location3;
    static double[][] forces4;
    static double[][] location4;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin_approx(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double cos_approx(double x) {
        double y_1 = _mod(x + PI, TWO_PI) - PI;
        double y2_1 = y_1 * y_1;
        double y4 = y2_1 * y2_1;
        double y6 = y4 * y2_1;
        return 1.0 - y2_1 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static double[] polar_force(double magnitude, double angle, boolean radian_mode) {
        double theta = radian_mode ? angle : angle * PI / 180.0;
        return new double[]{magnitude * cos_approx(theta), magnitude * sin_approx(theta)};
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static boolean in_static_equilibrium(double[][] forces, double[][] location, double eps) {
        double sum_moments = 0.0;
        int i = 0;
        int n = forces.length;
        while (i < n) {
            double[] r = ((double[])(location[i]));
            double[] f = ((double[])(forces[i]));
            double moment = r[0] * f[1] - r[1] * f[0];
            sum_moments = sum_moments + moment;
            i = i + 1;
        }
        return abs_float(sum_moments) < eps;
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        forces1 = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{-1.0, 2.0}}));
        location1 = ((double[][])(new double[][]{new double[]{1.0, 0.0}, new double[]{10.0, 0.0}}));
        System.out.println(_p(in_static_equilibrium(((double[][])(forces1)), ((double[][])(location1)), 0.1)));
        forces2 = ((double[][])(new double[][]{polar_force(718.4, 150.0, false), polar_force(879.54, 45.0, false), polar_force(100.0, -90.0, false)}));
        location2 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 0.0}, new double[]{0.0, 0.0}}));
        System.out.println(_p(in_static_equilibrium(((double[][])(forces2)), ((double[][])(location2)), 0.1)));
        forces3 = ((double[][])(new double[][]{polar_force(30.0 * 9.81, 15.0, false), polar_force(215.0, 135.0, false), polar_force(264.0, 60.0, false)}));
        location3 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 0.0}, new double[]{0.0, 0.0}}));
        System.out.println(_p(in_static_equilibrium(((double[][])(forces3)), ((double[][])(location3)), 0.1)));
        forces4 = ((double[][])(new double[][]{new double[]{0.0, -2000.0}, new double[]{0.0, -1200.0}, new double[]{0.0, 15600.0}, new double[]{0.0, -12400.0}}));
        location4 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{6.0, 0.0}, new double[]{10.0, 0.0}, new double[]{12.0, 0.0}}));
        System.out.println(_p(in_static_equilibrium(((double[][])(forces4)), ((double[][])(location4)), 0.1)));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
