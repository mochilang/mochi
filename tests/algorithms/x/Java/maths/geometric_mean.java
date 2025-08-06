public class Main {

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow_int(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double nth_root(double x, int n) {
        if (x == 0.0) {
            return 0.0;
        }
        double guess = x;
        int i_1 = 0;
        while (i_1 < 10) {
            double denom = pow_int(guess, n - 1);
            guess = (((Number)((n - 1))).doubleValue() * guess + x / denom) / (((Number)(n)).doubleValue());
            i_1 = i_1 + 1;
        }
        return guess;
    }

    static double round_nearest(double x) {
        if (x >= 0.0) {
            int n = ((Number)((x + 0.5))).intValue();
            return ((Number)(n)).doubleValue();
        }
        int n_1 = ((Number)((x - 0.5))).intValue();
        return ((Number)(n_1)).doubleValue();
    }

    static double compute_geometric_mean(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("no numbers"));
        }
        double product = 1.0;
        int i_2 = 0;
        while (i_2 < nums.length) {
            product = product * nums[i_2];
            i_2 = i_2 + 1;
        }
        if (product < 0.0 && Math.floorMod(nums.length, 2) == 0) {
            throw new RuntimeException(String.valueOf("Cannot Compute Geometric Mean for these numbers."));
        }
        double mean = nth_root(((Number)(Math.abs(product))).doubleValue(), nums.length);
        if (product < 0.0) {
            mean = -mean;
        }
        double possible = round_nearest(mean);
        if (pow_int(possible, nums.length) == product) {
            mean = possible;
        }
        return mean;
    }

    static void test_compute_geometric_mean() {
        double eps = 0.0001;
        double m1 = compute_geometric_mean(((double[])(new double[]{2.0, 8.0})));
        if (Math.abs(m1 - 4.0) > eps) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        double m2 = compute_geometric_mean(((double[])(new double[]{5.0, 125.0})));
        if (Math.abs(m2 - 25.0) > eps) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        double m3 = compute_geometric_mean(((double[])(new double[]{1.0, 0.0})));
        if (Math.abs(m3 - 0.0) > eps) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        double m4 = compute_geometric_mean(((double[])(new double[]{1.0, 5.0, 25.0, 5.0})));
        if (Math.abs(m4 - 5.0) > eps) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        double m5 = compute_geometric_mean(((double[])(new double[]{-5.0, 25.0, 1.0})));
        if (Math.abs(m5 + 5.0) > eps) {
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
