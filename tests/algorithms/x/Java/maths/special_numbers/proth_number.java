public class Main {

    static int pow2(int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static int proth(int number) {
        if (number < 1) {
            throw new RuntimeException(String.valueOf("Input value must be > 0"));
        }
        if (number == 1) {
            return 3;
        }
        if (number == 2) {
            return 5;
        }
        int temp = ((Number)((Math.floorDiv(number, 3)))).intValue();
        int pow = 1;
        int block_index = 1;
        while (pow <= temp) {
            pow = pow * 2;
            block_index = block_index + 1;
        }
        int[] proth_list = ((int[])(new int[]{3, 5}));
        int proth_index = 2;
        int increment = 3;
        int block = 1;
        while (block < block_index) {
            int i_1 = 0;
            while (i_1 < increment) {
                int next_val = pow2(block + 1) + proth_list[proth_index - 1];
                proth_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(proth_list), java.util.stream.IntStream.of(next_val)).toArray()));
                proth_index = proth_index + 1;
                i_1 = i_1 + 1;
            }
            increment = increment * 2;
            block = block + 1;
        }
        return proth_list[number - 1];
    }

    static void main() {
        int n = 1;
        while (n <= 10) {
            int value = proth(n);
            System.out.println("The " + _p(n) + "th Proth number: " + _p(value));
            n = n + 1;
        }
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
