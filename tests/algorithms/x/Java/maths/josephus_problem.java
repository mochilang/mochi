public class Main {
    static long r;

    static long josephus_recursive(long num_people, long step_size) {
        if (num_people <= 0 || step_size <= 0) {
            throw new RuntimeException(String.valueOf("num_people or step_size is not a positive integer."));
        }
        if (num_people == 1) {
            return 0;
        }
        return Math.floorMod((josephus_recursive(num_people - 1, step_size) + step_size), num_people);
    }

    static long find_winner(long num_people, long step_size) {
        return josephus_recursive(num_people, step_size) + 1;
    }

    static long[] remove_at(long[] xs, long idx) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 != idx) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(xs[(int)(i_1)])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static long josephus_iterative(long num_people, long step_size) {
        if (num_people <= 0 || step_size <= 0) {
            throw new RuntimeException(String.valueOf("num_people or step_size is not a positive integer."));
        }
        long[] circle_1 = ((long[])(new long[]{}));
        long i_3 = 1;
        while (i_3 <= num_people) {
            circle_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(circle_1), java.util.stream.LongStream.of(i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        long current_1 = 0;
        while (circle_1.length > 1) {
            current_1 = Math.floorMod((current_1 + step_size - 1), circle_1.length);
            circle_1 = ((long[])(remove_at(((long[])(circle_1)), current_1)));
        }
        return circle_1[(int)(0)];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r = josephus_recursive(7, 3);
            System.out.println(_p(r));
            System.out.println(_p(find_winner(7, 3)));
            System.out.println(_p(josephus_iterative(7, 3)));
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
