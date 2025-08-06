public class Main {

    static boolean is_prime(int number) {
        if (number < 0) {
            throw new RuntimeException(String.valueOf("is_prime() only accepts positive integers"));
        }
        if ((1 < number) && (number < 4)) {
            return true;
        } else         if ((number < 2) || (Math.floorMod(number, 2) == 0) || (Math.floorMod(number, 3) == 0)) {
            return false;
        }
        int i = 5;
        while (i * i <= number) {
            if ((Math.floorMod(number, i) == 0) || (Math.floorMod(number, (i + 2)) == 0)) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(is_prime(0)));
            System.out.println(_p(is_prime(1)));
            System.out.println(_p(is_prime(2)));
            System.out.println(_p(is_prime(3)));
            System.out.println(_p(is_prime(27)));
            System.out.println(_p(is_prime(87)));
            System.out.println(_p(is_prime(563)));
            System.out.println(_p(is_prime(2999)));
            System.out.println(_p(is_prime(67483)));
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
