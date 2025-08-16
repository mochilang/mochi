public class Main {
    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);
    static double[][] forces1;
    static double[][] location1 = ((double[][])(new double[][]{new double[]{1.0, 0.0}, new double[]{10.0, 0.0}}));
    static double[][] forces2;
    static double[][] location2 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 0.0}, new double[]{0.0, 0.0}}));
    static double[][] forces3;
    static double[][] location3 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{0.0, 0.0}, new double[]{0.0, 0.0}}));
    static double[][] forces4;
    static double[][] location4 = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{6.0, 0.0}, new double[]{10.0, 0.0}, new double[]{12.0, 0.0}}));

    static double _mod(double x, double m) {
        return (double)(x) - (double)((double)((((Number)(((Number)((double)(x) / (double)(m))).intValue())).doubleValue())) * (double)(m));
    }

    static double sin_approx(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y3_1 = (double)((double)(y2_1) * (double)(y));
        double y5_1 = (double)((double)(y3_1) * (double)(y2_1));
        double y7_1 = (double)((double)(y5_1) * (double)(y2_1));
        return (double)((double)((double)(y) - (double)((double)(y3_1) / (double)(6.0))) + (double)((double)(y5_1) / (double)(120.0))) - (double)((double)(y7_1) / (double)(5040.0));
    }

    static double cos_approx(double x) {
        double y_1 = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_3 = (double)((double)(y_1) * (double)(y_1));
        double y4_1 = (double)((double)(y2_3) * (double)(y2_3));
        double y6_1 = (double)((double)(y4_1) * (double)(y2_3));
        return (double)((double)((double)(1.0) - (double)((double)(y2_3) / (double)(2.0))) + (double)((double)(y4_1) / (double)(24.0))) - (double)((double)(y6_1) / (double)(720.0));
    }

    static double[] polar_force(double magnitude, double angle, boolean radian_mode) {
        double theta = (double)(radian_mode ? angle : (double)((double)(angle) * (double)(PI)) / (double)(180.0));
        return new double[]{(double)(magnitude) * (double)(cos_approx((double)(theta))), (double)(magnitude) * (double)(sin_approx((double)(theta)))};
    }

    static double abs_float(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        } else {
            return x;
        }
    }

    static boolean in_static_equilibrium(double[][] forces, double[][] location, double eps) {
        double sum_moments = (double)(0.0);
        long i_1 = 0L;
        long n_1 = (long)(forces.length);
        while ((long)(i_1) < (long)(n_1)) {
            double[] r_1 = ((double[])(location[(int)((long)(i_1))]));
            double[] f_1 = ((double[])(forces[(int)((long)(i_1))]));
            double moment_1 = (double)((double)((double)(r_1[(int)((long)(0))]) * (double)(f_1[(int)((long)(1))])) - (double)((double)(r_1[(int)((long)(1))]) * (double)(f_1[(int)((long)(0))])));
            sum_moments = (double)((double)(sum_moments) + (double)(moment_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (double)(abs_float((double)(sum_moments))) < (double)(eps);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            forces1 = ((double[][])(new double[][]{new double[]{1.0, 1.0}, new double[]{-1.0, 2.0}}));
            System.out.println(_p(in_static_equilibrium(((double[][])(forces1)), ((double[][])(location1)), (double)(0.1))));
            forces2 = ((double[][])(new double[][]{polar_force((double)(718.4), (double)(150.0), false), polar_force((double)(879.54), (double)(45.0), false), polar_force((double)(100.0), (double)(-90.0), false)}));
            System.out.println(_p(in_static_equilibrium(((double[][])(forces2)), ((double[][])(location2)), (double)(0.1))));
            forces3 = ((double[][])(new double[][]{polar_force((double)((double)(30.0) * (double)(9.81)), (double)(15.0), false), polar_force((double)(215.0), (double)(135.0), false), polar_force((double)(264.0), (double)(60.0), false)}));
            System.out.println(_p(in_static_equilibrium(((double[][])(forces3)), ((double[][])(location3)), (double)(0.1))));
            forces4 = ((double[][])(new double[][]{new double[]{0.0, -2000.0}, new double[]{0.0, -1200.0}, new double[]{0.0, 15600.0}, new double[]{0.0, -12400.0}}));
            System.out.println(_p(in_static_equilibrium(((double[][])(forces4)), ((double[][])(location4)), (double)(0.1))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
