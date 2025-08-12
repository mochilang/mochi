public class Main {

    static double to_float(long x) {
        return x * 1.0;
    }

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 10) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static long floor(double x) {
        long n = 0;
        double y_1 = x;
        while (y_1 >= 1.0) {
            y_1 = y_1 - 1.0;
            n = n + 1;
        }
        return n;
    }

    static long[] juggler_sequence(long n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("number must be a positive integer"));
        }
        long[] seq_1 = ((long[])(new long[]{n}));
        long current_1 = n;
        while (current_1 != 1) {
            if (Math.floorMod(current_1, 2) == 0) {
                current_1 = floor(sqrt(to_float(current_1)));
            } else {
                double r_1 = sqrt(to_float(current_1));
                current_1 = floor(r_1 * r_1 * r_1);
            }
            seq_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(seq_1), java.util.stream.LongStream.of(current_1)).toArray()));
        }
        return seq_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(juggler_sequence(3)));
            System.out.println(_p(juggler_sequence(10)));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
