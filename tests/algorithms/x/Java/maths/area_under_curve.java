public class Main {
    static long i_2 = 0;

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static double trapezoidal_area(java.util.function.Function<Double,Double> f, double x_start, double x_end, long steps) {
        double step = (x_end - x_start) / (((Number)(steps)).doubleValue());
        double x1_1 = x_start;
        double fx1_1 = f.apply(x_start);
        double area_1 = 0.0;
        long i_1 = 0;
        while (i_1 < steps) {
            double x2_1 = x1_1 + step;
            double fx2_1 = f.apply(x2_1);
            area_1 = area_1 + abs_float(fx2_1 + fx1_1) * step / 2.0;
            x1_1 = x2_1;
            fx1_1 = fx2_1;
            i_1 = i_1 + 1;
        }
        return area_1;
    }

    static double f(double x) {
        return x * x * x + x * x;
    }
    public static void main(String[] args) {
        System.out.println("f(x) = x^3 + x^2");
        System.out.println("The area between the curve, x = -5, x = 5 and the x axis is:");
        i_2 = 10;
        while (i_2 <= 100000) {
            double result = trapezoidal_area(Main::f, -5.0, 5.0, i_2);
            System.out.println("with " + _p(i_2) + " steps: " + _p(result));
            i_2 = i_2 * 10;
        }
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
