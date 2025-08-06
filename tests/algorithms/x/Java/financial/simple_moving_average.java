public class Main {
    static class SMAValue {
        double value;
        boolean ok;
        SMAValue(double value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        SMAValue() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }

    static double[] data;
    static int window_size;
    static SMAValue[] sma_values;
    static int idx = 0;

    static SMAValue[] simple_moving_average(double[] data, int window_size) {
        if (window_size < 1) {
            throw new RuntimeException(String.valueOf("Window size must be a positive integer"));
        }
        SMAValue[] result = ((SMAValue[])(new SMAValue[]{}));
        double window_sum = 0.0;
        int i = 0;
        while (i < data.length) {
            window_sum = window_sum + data[i];
            if (i >= window_size) {
                window_sum = window_sum - data[i - window_size];
            }
            if (i >= window_size - 1) {
                double avg = window_sum / window_size;
                result = ((SMAValue[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new SMAValue(avg, true))).toArray(SMAValue[]::new)));
            } else {
                result = ((SMAValue[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new SMAValue(0.0, false))).toArray(SMAValue[]::new)));
            }
            i = i + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data = ((double[])(new double[]{10.0, 12.0, 15.0, 13.0, 14.0, 16.0, 18.0, 17.0, 19.0, 21.0}));
            window_size = 3;
            sma_values = ((SMAValue[])(simple_moving_average(((double[])(data)), window_size)));
            idx = 0;
            while (idx < sma_values.length) {
                SMAValue item = sma_values[idx];
                if (item.ok) {
                    System.out.println("Day " + _p(idx + 1) + ": " + _p(item.value));
                } else {
                    System.out.println("Day " + _p(idx + 1) + ": Not enough data for SMA");
                }
                idx = idx + 1;
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
