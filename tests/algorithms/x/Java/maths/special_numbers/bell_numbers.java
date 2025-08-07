public class Main {

    static int binomial_coefficient(int total_elements, int elements_to_choose) {
        if (elements_to_choose == 0 || elements_to_choose == total_elements) {
            return 1;
        }
        int k = elements_to_choose;
        if (k > total_elements - k) {
            k = total_elements - k;
        }
        int coefficient = 1;
        int i = 0;
        while (i < k) {
            coefficient = coefficient * (total_elements - i);
            coefficient = Math.floorDiv(coefficient, (i + 1));
            i = i + 1;
        }
        return coefficient;
    }

    static int[] bell_numbers(int max_set_length) {
        if (max_set_length < 0) {
            throw new RuntimeException(String.valueOf("max_set_length must be non-negative"));
        }
        int[] bell = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 <= max_set_length) {
            bell = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bell), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
bell[0] = 1;
        i_1 = 1;
        while (i_1 <= max_set_length) {
            int j = 0;
            while (j < i_1) {
bell[i_1] = bell[i_1] + binomial_coefficient(i_1 - 1, j) * bell[j];
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return bell;
    }

    static void main() {
        System.out.println(_p(bell_numbers(5)));
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
        return String.valueOf(v);
    }
}
