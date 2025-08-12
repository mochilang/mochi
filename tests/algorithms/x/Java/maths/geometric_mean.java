public class Main {

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow_int(double base, long exp) {
        double result = 1.0;
        long i_1 = 0;
        while (i_1 < exp) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double nth_root(double x, long n) {
        if (x == 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_3 = 0;
        while (i_3 < 10) {
            double denom_1 = pow_int(guess_1, n - 1);
            guess_1 = (((Number)((n - 1))).doubleValue() * guess_1 + x / denom_1) / (((Number)(n)).doubleValue());
            i_3 = i_3 + 1;
        }
        return guess_1;
    }

    static double round_nearest(double x) {
        if (x >= 0.0) {
            long n = ((Number)((x + 0.5))).intValue();
            return ((Number)(n)).doubleValue();
        }
        long n_2 = ((Number)((x - 0.5))).intValue();
        return ((Number)(n_2)).doubleValue();
    }

    static double compute_geometric_mean(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("no numbers"));
        }
        double product_1 = 1.0;
        long i_5 = 0;
        while (i_5 < nums.length) {
            product_1 = product_1 * nums[(int)(i_5)];
            i_5 = i_5 + 1;
        }
        if (product_1 < 0.0 && Math.floorMod(nums.length, 2) == 0) {
            throw new RuntimeException(String.valueOf("Cannot Compute Geometric Mean for these numbers."));
        }
        double mean_1 = nth_root(((Number)(Math.abs(product_1))).doubleValue(), nums.length);
        if (product_1 < 0.0) {
            mean_1 = -mean_1;
        }
        double possible_1 = round_nearest(mean_1);
        if (pow_int(possible_1, nums.length) == product_1) {
            mean_1 = possible_1;
        }
        return mean_1;
    }

    static void test_compute_geometric_mean() {
        double eps = 0.0001;
        double m1_1 = compute_geometric_mean(((double[])(new double[]{2.0, 8.0})));
        if (Math.abs(m1_1 - 4.0) > eps) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        double m2_1 = compute_geometric_mean(((double[])(new double[]{5.0, 125.0})));
        if (Math.abs(m2_1 - 25.0) > eps) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        double m3_1 = compute_geometric_mean(((double[])(new double[]{1.0, 0.0})));
        if (Math.abs(m3_1 - 0.0) > eps) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        double m4_1 = compute_geometric_mean(((double[])(new double[]{1.0, 5.0, 25.0, 5.0})));
        if (Math.abs(m4_1 - 5.0) > eps) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        double m5_1 = compute_geometric_mean(((double[])(new double[]{-5.0, 25.0, 1.0})));
        if (Math.abs(m5_1 + 5.0) > eps) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
    }

    static void main() {
        test_compute_geometric_mean();
        System.out.println(compute_geometric_mean(((double[])(new double[]{-3.0, -27.0}))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
