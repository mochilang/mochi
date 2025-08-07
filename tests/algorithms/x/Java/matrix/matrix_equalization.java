public class Main {

    static int[] unique(int[] nums) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < nums.length) {
            int v = nums[i];
            boolean found = false;
            int j = 0;
            while (j < res.length) {
                if (res[j] == v) {
                    found = true;
                    break;
                }
                j = j + 1;
            }
            if (!found) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(v)).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int array_equalization(int[] vector, int step_size) {
        if (step_size <= 0) {
            throw new RuntimeException(String.valueOf("Step size must be positive and non-zero."));
        }
        int[] elems = ((int[])(unique(((int[])(vector)))));
        int min_updates = vector.length;
        int i_1 = 0;
        while (i_1 < elems.length) {
            int target = elems[i_1];
            int idx = 0;
            int updates = 0;
            while (idx < vector.length) {
                if (vector[idx] != target) {
                    updates = updates + 1;
                    idx = idx + step_size;
                } else {
                    idx = idx + 1;
                }
            }
            if (updates < min_updates) {
                min_updates = updates;
            }
            i_1 = i_1 + 1;
        }
        return min_updates;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(array_equalization(((int[])(new int[]{1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2})), 4)));
            System.out.println(_p(array_equalization(((int[])(new int[]{22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92})), 2)));
            System.out.println(_p(array_equalization(((int[])(new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})), 5)));
            System.out.println(_p(array_equalization(((int[])(new int[]{22, 22, 22, 33, 33, 33})), 2)));
            System.out.println(_p(array_equalization(((int[])(new int[]{1, 2, 3})), 2147483647)));
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
