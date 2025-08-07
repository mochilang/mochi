public class Main {
    static int seed = 0;
    static int[] integers;
    static String[] strings;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return Math.floorDiv(seed, 65536);
    }

    static int randint(int a, int b) {
        int r = rand();
        return a + Math.floorMod(r, (b - a + 1));
    }

    static int[] fisher_yates_shuffle_int(int[] data) {
        int[] res = ((int[])(data));
        int i = 0;
        while (i < res.length) {
            int a = randint(0, res.length - 1);
            int b = randint(0, res.length - 1);
            int temp = res[a];
res[a] = res[b];
res[b] = temp;
            i = i + 1;
        }
        return res;
    }

    static String[] fisher_yates_shuffle_str(String[] data) {
        String[] res_1 = ((String[])(data));
        int i_1 = 0;
        while (i_1 < res_1.length) {
            int a_1 = randint(0, res_1.length - 1);
            int b_1 = randint(0, res_1.length - 1);
            String temp_1 = res_1[a_1];
res_1[a_1] = res_1[b_1];
res_1[b_1] = temp_1;
            i_1 = i_1 + 1;
        }
        return res_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
            integers = ((int[])(new int[]{0, 1, 2, 3, 4, 5, 6, 7}));
            strings = ((String[])(new String[]{"python", "says", "hello", "!"}));
            System.out.println("Fisher-Yates Shuffle:");
            System.out.println("List " + _p(integers) + " " + _p(strings));
            System.out.println("FY Shuffle " + _p(fisher_yates_shuffle_int(((int[])(integers)))) + " " + _p(fisher_yates_shuffle_str(((String[])(strings)))));
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
