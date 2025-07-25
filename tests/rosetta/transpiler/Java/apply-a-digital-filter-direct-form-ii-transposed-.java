public class Main {
    static double[] a = new double[]{1.0, -2.7756e-16, 0.33333333, -1.85e-17};
    static double[] b = new double[]{0.16666667, 0.5, 0.5, 0.16666667};
    static double[] sig = new double[]{-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589};
    static double[] res = applyFilter(sig, a, b);
    static int k = 0;

    static double[] applyFilter(double[] input, double[] a, double[] b) {
        double[] out = new double[]{};
        double scale = 1.0 / a[0];
        int i = 0;
        while (i < input.length) {
            double tmp = 0.0;
            int j = 0;
            while (j <= i && j < b.length) {
                tmp = tmp + b[j] * ((Number)(input[i - j])).doubleValue();
                j = j + 1;
            }
            j = 0;
            while (j < i && j + 1 < a.length) {
                tmp = tmp - a[j + 1] * out[i - j - 1];
                j = j + 1;
            }
            out = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(out), java.util.stream.DoubleStream.of(tmp * scale)).toArray();
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (k < res.length) {
                System.out.println(res[k]);
                k = k + 1;
            }
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
