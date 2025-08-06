public class Main {

    static double exp_approx(double x) {
        double term = 1.0;
        double sum = 1.0;
        int i = 1;
        while (i < 20) {
            term = term * x / (((Number)(i)).doubleValue());
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double[] softmax(double[] vec) {
        double[] exps = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < vec.length) {
            exps = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(exps), java.util.stream.DoubleStream.of(exp_approx(vec[i_1]))).toArray()));
            i_1 = i_1 + 1;
        }
        double total = 0.0;
        i_1 = 0;
        while (i_1 < exps.length) {
            total = total + exps[i_1];
            i_1 = i_1 + 1;
        }
        double[] result = ((double[])(new double[]{}));
        i_1 = 0;
        while (i_1 < exps.length) {
            result = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(result), java.util.stream.DoubleStream.of(exps[i_1] / total)).toArray()));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double abs_val(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean approx_equal(double a, double b) {
        return abs_val(a - b) < 0.0001;
    }

    static void test_softmax() {
        double[] s1 = ((double[])(softmax(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
        double sum1 = 0.0;
        int i_2 = 0;
        while (i_2 < s1.length) {
            sum1 = sum1 + s1[i_2];
            i_2 = i_2 + 1;
        }
        if (!(Boolean)approx_equal(sum1, 1.0)) {
            throw new RuntimeException(String.valueOf("sum test failed"));
        }
        double[] s2 = ((double[])(softmax(((double[])(new double[]{5.0, 5.0})))));
        if (!(((Boolean)(approx_equal(s2[0], 0.5))) && ((Boolean)(approx_equal(s2[1], 0.5))))) {
            throw new RuntimeException(String.valueOf("equal elements test failed"));
        }
        double[] s3 = ((double[])(softmax(((double[])(new double[]{0.0})))));
        if (!(Boolean)approx_equal(s3[0], 1.0)) {
            throw new RuntimeException(String.valueOf("zero vector test failed"));
        }
    }

    static void main() {
        test_softmax();
        System.out.println(_p(softmax(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
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
