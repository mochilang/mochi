public class Main {

    static int binary_multiply(int a, int b) {
        int x = a;
        int y = b;
        int res = 0;
        while (y > 0) {
            if (Math.floorMod(y, 2) == 1) {
                res = res + x;
            }
            x = x + x;
            y = ((Number)((y / 2))).intValue();
        }
        return res;
    }

    static int binary_mod_multiply(int a, int b, int modulus) {
        int x_1 = a;
        int y_1 = b;
        int res_1 = 0;
        while (y_1 > 0) {
            if (Math.floorMod(y_1, 2) == 1) {
                res_1 = Math.floorMod(((Math.floorMod(res_1, modulus)) + (Math.floorMod(x_1, modulus))), modulus);
            }
            x_1 = x_1 + x_1;
            y_1 = ((Number)((y_1 / 2))).intValue();
        }
        return Math.floorMod(res_1, modulus);
    }

    static void main() {
        System.out.println(_p(binary_multiply(2, 3)));
        System.out.println(_p(binary_multiply(5, 0)));
        System.out.println(_p(binary_mod_multiply(2, 3, 5)));
        System.out.println(_p(binary_mod_multiply(10, 5, 13)));
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
