public class Main {
    static double PI;
    static int rand_seed = 0;

    static double rand_float() {
        rand_seed = ((int)(Math.floorMod(((long)((1103515245 * rand_seed + 12345))), 2147483648L)));
        return (((Number)(rand_seed)).doubleValue()) / 2147483648.0;
    }

    static double rand_range(double min_val, double max_val) {
        return rand_float() * (max_val - min_val) + min_val;
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x == 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static void pi_estimator(int iterations) {
        double inside = 0.0;
        int i_1 = 0;
        while (i_1 < iterations) {
            double x = rand_range(-1.0, 1.0);
            double y = rand_range(-1.0, 1.0);
            if (x * x + y * y <= 1.0) {
                inside = inside + 1.0;
            }
            i_1 = i_1 + 1;
        }
        double proportion = inside / (((Number)(iterations)).doubleValue());
        double pi_estimate = proportion * 4.0;
        System.out.println("The estimated value of pi is" + " " + String.valueOf(pi_estimate));
        System.out.println("The numpy value of pi is" + " " + String.valueOf(PI));
        System.out.println("The total error is" + " " + String.valueOf(abs_float(PI - pi_estimate)));
    }

    static double area_under_curve_estimator(int iterations, java.util.function.Function<Double,Double> f, double min_value, double max_value) {
        double sum = 0.0;
        int i_2 = 0;
        while (i_2 < iterations) {
            double x_1 = rand_range(min_value, max_value);
            sum = sum + f.apply(x_1);
            i_2 = i_2 + 1;
        }
        double expected = sum / (((Number)(iterations)).doubleValue());
        return expected * (max_value - min_value);
    }

    static void area_under_line_estimator_check(int iterations, double min_value, double max_value) {
        java.util.function.Function<Double,Double>[] identity_function = new java.util.function.Function[1];
        identity_function[0] = (x_2) -> x_2;
        double estimated_value = area_under_curve_estimator(iterations, identity_function[0], min_value, max_value);
        double expected_value = (max_value * max_value - min_value * min_value) / 2.0;
        System.out.println("******************");
        System.out.println("Estimating area under y=x where x varies from" + " " + String.valueOf(min_value));
        System.out.println("Estimated value is" + " " + String.valueOf(estimated_value));
        System.out.println("Expected value is" + " " + String.valueOf(expected_value));
        System.out.println("Total error is" + " " + String.valueOf(abs_float(estimated_value - expected_value)));
        System.out.println("******************");
    }

    static void pi_estimator_using_area_under_curve(int iterations) {
        java.util.function.Function<Double,Double>[] semi_circle = new java.util.function.Function[1];
        semi_circle[0] = (x_3) -> {
        double y_1 = 4.0 - x_3 * x_3;
        double s = sqrtApprox(y_1);
        return s;
};
        double estimated_value_1 = area_under_curve_estimator(iterations, semi_circle[0], 0.0, 2.0);
        System.out.println("******************");
        System.out.println("Estimating pi using area_under_curve_estimator");
        System.out.println("Estimated value is" + " " + String.valueOf(estimated_value_1));
        System.out.println("Expected value is" + " " + String.valueOf(PI));
        System.out.println("Total error is" + " " + String.valueOf(abs_float(estimated_value_1 - PI)));
        System.out.println("******************");
    }

    static void main() {
        pi_estimator(1000);
        area_under_line_estimator_check(1000, 0.0, 1.0);
        pi_estimator_using_area_under_curve(1000);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            rand_seed = 123456789;
            main();
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
}
