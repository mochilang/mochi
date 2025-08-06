public class Main {

    static int multiplicative_persistence(int num) {
        if (num < 0) {
            throw new RuntimeException(String.valueOf("multiplicative_persistence() does not accept negative values"));
        }
        int steps = 0;
        int n = num;
        while (n >= 10) {
            int product = 1;
            int temp = n;
            while (temp > 0) {
                int digit = Math.floorMod(temp, 10);
                product = product * digit;
                temp = temp / 10;
            }
            n = product;
            steps = steps + 1;
        }
        return steps;
    }

    static int additive_persistence(int num) {
        if (num < 0) {
            throw new RuntimeException(String.valueOf("additive_persistence() does not accept negative values"));
        }
        int steps_1 = 0;
        int n_1 = num;
        while (n_1 >= 10) {
            int total = 0;
            int temp_1 = n_1;
            while (temp_1 > 0) {
                int digit_1 = Math.floorMod(temp_1, 10);
                total = total + digit_1;
                temp_1 = temp_1 / 10;
            }
            n_1 = total;
            steps_1 = steps_1 + 1;
        }
        return steps_1;
    }

    static void test_persistence() {
        if (multiplicative_persistence(217) != 2) {
            throw new RuntimeException(String.valueOf("multiplicative_persistence failed"));
        }
        if (additive_persistence(199) != 3) {
            throw new RuntimeException(String.valueOf("additive_persistence failed"));
        }
    }

    static void main() {
        test_persistence();
        System.out.println(_p(multiplicative_persistence(217)));
        System.out.println(_p(additive_persistence(199)));
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
