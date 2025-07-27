public class Main {
    static double[] dxs = new double[]{-0.533, 0.27, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275, 1.251, -0.231, -0.401, 0.269, 0.491, 0.951, 1.15, 0.001, -0.382, 0.161, 0.915, 2.08, -2.337, 0.034, -0.126, 0.014, 0.709, 0.129, -1.093, -0.483, -1.193, 0.02, -0.051, 0.047, -0.095, 0.695, 0.34, -0.182, 0.287, 0.213, -0.423, -0.021, -0.134, 1.798, 0.021, -1.099, -0.361, 1.636, -1.134, 1.315, 0.201, 0.034, 0.097, -0.17, 0.054, -0.553, -0.024, -0.181, -0.7, -0.361, -0.789, 0.279, -0.174, -0.009, -0.323, -0.658, 0.348, -0.528, 0.881, 0.021, -0.853, 0.157, 0.648, 1.774, -1.043, 0.051, 0.021, 0.247, -0.31, 0.171, 0.0, 0.106, 0.024, -0.386, 0.962, 0.765, -0.125, -0.289, 0.521, 0.017, 0.281, -0.749, -0.149, -2.436, -0.909, 0.394, -0.113, -0.598, 0.443, -0.521, -0.799, 0.087};
    static double[] dys = new double[]{0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395, 0.49, -0.682, -0.065, 0.242, -0.288, 0.658, 0.459, 0.0, 0.426, 0.205, -0.765, -2.188, -0.742, -0.01, 0.089, 0.208, 0.585, 0.633, -0.444, -0.351, -1.087, 0.199, 0.701, 0.096, -0.025, -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007, 0.009, 0.508, -0.79, 0.723, 0.881, -0.508, 0.393, -0.226, 0.71, 0.038, -0.217, 0.831, 0.48, 0.407, 0.447, -0.295, 1.126, 0.38, 0.549, -0.445, -0.046, 0.428, -0.074, 0.217, -0.822, 0.491, 1.347, -0.141, 1.23, -0.044, 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, -0.729, 0.65, -1.103, 0.154, -1.72, 0.051, -0.385, 0.477, 1.537, -0.901, 0.939, -0.411, 0.341, -0.411, 0.106, 0.224, -0.947, -1.424, -0.542, -1.032};

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double g = x;
        int i = 0;
        while (i < 20) {
            g = (g + x / g) / 2.0;
            i = i + 1;
        }
        return g;
    }

    static double[] funnel(double[] fa, java.util.function.BiFunction<Double,Double,Double> r) {
        double x = 0.0;
        double[] result = new double[]{};
        int i = 0;
        while (i < fa.length) {
            double f = fa[i];
            result = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(x + f)).toArray();
            x = r.apply(x, f);
            i = i + 1;
        }
        return result;
    }

    static double mean(double[] fa) {
        double sum = 0.0;
        int i = 0;
        while (i < fa.length) {
            sum = sum + fa[i];
            i = i + 1;
        }
        return sum / (((Number)(fa.length)).doubleValue());
    }

    static double stdDev(double[] fa) {
        double m = mean(fa);
        double sum = 0.0;
        int i = 0;
        while (i < fa.length) {
            double d = fa[i] - m;
            sum = sum + d * d;
            i = i + 1;
        }
        double r = sqrtApprox(sum / (((Number)(fa.length)).doubleValue()));
        return r;
    }

    static void experiment(String label, java.util.function.BiFunction<Double,Double,Double> r) {
        double[] rxs = funnel(dxs, r);
        double[] rys = funnel(dys, r);
        System.out.println(label + "  :      x        y");
        System.out.println("Mean    :  " + String.valueOf(mean(rxs)) + ", " + String.valueOf(mean(rys)));
        System.out.println("Std Dev :  " + String.valueOf(stdDev(rxs)) + ", " + String.valueOf(stdDev(rys)));
        System.out.println("");
    }

    static void main() {
        experiment("Rule 1", (x, y) -> 0.0);
        experiment("Rule 2", (x, dz) -> -dz);
        experiment("Rule 3", (z, dz) -> -(z + dz));
        experiment("Rule 4", (z, dz) -> z + dz);
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
