public class Main {

    static boolean is_harmonic_series(double[] series) {
        if (series.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        if (series.length == 1) {
            if (series[0] == 0.0) {
                throw new RuntimeException(String.valueOf("Input series cannot have 0 as an element"));
            }
            return true;
        }
        double[] rec_series = ((double[])(new double[]{}));
        int i = 0;
        while (i < series.length) {
            double val = series[i];
            if (val == 0.0) {
                throw new RuntimeException(String.valueOf("Input series cannot have 0 as an element"));
            }
            rec_series = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(rec_series), java.util.stream.DoubleStream.of(1.0 / val)).toArray()));
            i = i + 1;
        }
        double common_diff = rec_series[1] - rec_series[0];
        int idx = 2;
        while (idx < rec_series.length) {
            if (rec_series[idx] - rec_series[idx - 1] != common_diff) {
                return false;
            }
            idx = idx + 1;
        }
        return true;
    }

    static double harmonic_mean(double[] series) {
        if (series.length == 0) {
            throw new RuntimeException(String.valueOf("Input list must be a non empty list"));
        }
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < series.length) {
            total = total + 1.0 / series[i_1];
            i_1 = i_1 + 1;
        }
        return (((Number)(series.length)).doubleValue()) / total;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(is_harmonic_series(((double[])(new double[]{1.0, 2.0 / 3.0, 1.0 / 2.0, 2.0 / 5.0, 1.0 / 3.0}))));
            System.out.println(is_harmonic_series(((double[])(new double[]{1.0, 2.0 / 3.0, 2.0 / 5.0, 1.0 / 3.0}))));
            System.out.println(harmonic_mean(((double[])(new double[]{1.0, 4.0, 4.0}))));
            System.out.println(harmonic_mean(((double[])(new double[]{3.0, 6.0, 9.0, 12.0}))));
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
