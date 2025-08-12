public class Main {

    static long[] binary_step(double[] vector) {
        long[] out = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vector.length)) {
            if ((double)(_getd(vector, (int)((long)(i_1)))) >= 0.0) {
                out = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(out), java.util.stream.LongStream.of(1L)).toArray()));
            } else {
                out = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(out), java.util.stream.LongStream.of(0L)).toArray()));
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return out;
    }

    static void main() {
        double[] vector = ((double[])(new double[]{-1.2, 0.0, 2.0, 1.45, -3.7, 0.3}));
        long[] result_1 = ((long[])(binary_step(((double[])(vector)))));
        System.out.println(java.util.Arrays.toString(result_1));
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

    static double _getd(double[] a, int i) {
        if (a == null) return 0.0;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0.0;
        return a[i];
    }
}
