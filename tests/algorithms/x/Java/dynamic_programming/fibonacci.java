public class Main {
    static class Fibonacci {
        long[] sequence;
        Fibonacci(long[] sequence) {
            this.sequence = sequence;
        }
        Fibonacci() {}
        @Override public String toString() {
            return String.format("{'sequence': %s}", String.valueOf(sequence));
        }
    }

    static class FibGetResult {
        Fibonacci fib;
        long[] values;
        FibGetResult(Fibonacci fib, long[] values) {
            this.fib = fib;
            this.values = values;
        }
        FibGetResult() {}
        @Override public String toString() {
            return String.format("{'fib': %s, 'values': %s}", String.valueOf(fib), String.valueOf(values));
        }
    }


    static Fibonacci create_fibonacci() {
        return new Fibonacci(new long[]{0, 1});
    }

    static FibGetResult fib_get(Fibonacci f, long index) {
        long[] seq = ((long[])(f.sequence));
        while ((long)(seq.length) < index) {
            long next_1 = (long)((long)(seq[(int)((long)((long)(seq.length) - (long)(1)))]) + (long)(seq[(int)((long)((long)(seq.length) - (long)(2)))]));
            seq = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(seq), java.util.stream.LongStream.of((long)(next_1))).toArray()));
        }
f.sequence = seq;
        long[] result_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < index) {
            result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(seq[(int)((long)(i_1))]))).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return new FibGetResult(f, result_1);
    }

    static void main() {
        Fibonacci fib = create_fibonacci();
        FibGetResult res_1 = fib_get(fib, 10L);
        fib = res_1.fib;
        System.out.println(_p(res_1.values));
        res_1 = fib_get(fib, 5L);
        fib = res_1.fib;
        System.out.println(_p(res_1.values));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
