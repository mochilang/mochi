public class Main {

    static int floor_div(int a, int b) {
        int q = a / b;
        int r = Math.floorMod(a, b);
        if (r != 0 && ((a < 0 && b > 0) || (a > 0 && b < 0))) {
            q = q - 1;
        }
        return q;
    }

    static int[] continued_fraction(int numerator, int denominator) {
        int num = numerator;
        int den = denominator;
        int[] result = ((int[])(new int[]{}));
        while (true) {
            int integer_part = floor_div(num, den);
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(integer_part)).toArray()));
            num = num - integer_part * den;
            if (num == 0) {
                break;
            }
            int tmp = num;
            num = den;
            den = tmp;
        }
        return result;
    }

    static String list_to_string(int[] lst) {
        String s = "[";
        int i = 0;
        while (i < lst.length) {
            s = s + _p(_geti(lst, i));
            if (i < lst.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        return s + "]";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Continued Fraction of 0.84375 is: " + String.valueOf(list_to_string(((int[])(continued_fraction(27, 32))))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
