public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int r = Math.floorMod(x, y);
            x = y;
            y = r;
        }
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int get_greatest_common_divisor(int[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("at least one number is required"));
        }
        int g = nums[0];
        if (g <= 0) {
            throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
        }
        int i = 1;
        while (i < nums.length) {
            int n = nums[i];
            if (n <= 0) {
                throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
            }
            g = gcd(g, n);
            i = i + 1;
        }
        return g;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(get_greatest_common_divisor(((int[])(new int[]{18, 45})))));
            System.out.println(_p(get_greatest_common_divisor(((int[])(new int[]{23, 37})))));
            System.out.println(_p(get_greatest_common_divisor(((int[])(new int[]{2520, 8350})))));
            System.out.println(_p(get_greatest_common_divisor(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))));
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
