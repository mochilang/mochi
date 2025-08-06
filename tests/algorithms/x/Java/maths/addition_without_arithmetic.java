public class Main {
    static int MAX;
    static int HALF;

    static int to_unsigned(int n) {
        if (n < 0) {
            return MAX + n;
        }
        return n;
    }

    static int from_unsigned(int n) {
        if (n >= HALF) {
            return n - MAX;
        }
        return n;
    }

    static int bit_and(int a, int b) {
        int x = a;
        int y = b;
        int res = 0;
        int bit = 1;
        int i = 0;
        while (i < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y, 2) == 1)) {
                res = res + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
            i = i + 1;
        }
        return res;
    }

    static int bit_xor(int a, int b) {
        int x_1 = a;
        int y_1 = b;
        int res_1 = 0;
        int bit_1 = 1;
        int i_1 = 0;
        while (i_1 < 32) {
            int abit = Math.floorMod(x_1, 2);
            int bbit = Math.floorMod(y_1, 2);
            if (Math.floorMod((abit + bbit), 2) == 1) {
                res_1 = res_1 + bit_1;
            }
            x_1 = x_1 / 2;
            y_1 = y_1 / 2;
            bit_1 = bit_1 * 2;
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static int lshift1(int num) {
        return Math.floorMod((num * 2), MAX);
    }

    static int add(int a, int b) {
        int first = to_unsigned(a);
        int second = to_unsigned(b);
        while (second != 0) {
            int carry = bit_and(first, second);
            first = bit_xor(first, second);
            second = lshift1(carry);
        }
        int result = from_unsigned(first);
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            MAX = (int)4294967296L;
            HALF = (int)2147483648L;
            System.out.println(_p(add(3, 5)));
            System.out.println(_p(add(13, 5)));
            System.out.println(_p(add(-7, 2)));
            System.out.println(_p(add(0, -7)));
            System.out.println(_p(add(-321, 0)));
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
