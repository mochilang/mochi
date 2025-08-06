public class Main {

    static int[] make_list(int len, int value) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < len) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static int min_steps_to_one(int number) {
        if (number <= 0) {
            return 0;
        }
        int[] table = ((int[])(make_list(number + 1, number + 1)));
table[1] = 0;
        int i_1 = 1;
        while (i_1 < number) {
table[i_1 + 1] = min_int(table[i_1 + 1], table[i_1] + 1);
            if (i_1 * 2 <= number) {
table[i_1 * 2] = min_int(table[i_1 * 2], table[i_1] + 1);
            }
            if (i_1 * 3 <= number) {
table[i_1 * 3] = min_int(table[i_1 * 3], table[i_1] + 1);
            }
            i_1 = i_1 + 1;
        }
        return table[number];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(min_steps_to_one(10)));
            System.out.println(_p(min_steps_to_one(15)));
            System.out.println(_p(min_steps_to_one(6)));
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
