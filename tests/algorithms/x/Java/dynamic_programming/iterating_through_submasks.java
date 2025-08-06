public class Main {

    static int bitwise_and(int a, int b) {
        int result = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int abit = Math.floorMod(x, 2);
            int bbit = Math.floorMod(y, 2);
            if (abit == 1 && bbit == 1) {
                result = result + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
        }
        return result;
    }

    static int[] list_of_submasks(int mask) {
        if (mask <= 0) {
            throw new RuntimeException(String.valueOf("mask needs to be positive integer, your input " + _p(mask)));
        }
        int[] all_submasks = ((int[])(new int[]{}));
        int submask = mask;
        while (submask != 0) {
            all_submasks = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(all_submasks), java.util.stream.IntStream.of(submask)).toArray()));
            submask = bitwise_and(submask - 1, mask);
        }
        return all_submasks;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(list_of_submasks(15)));
            System.out.println(_p(list_of_submasks(13)));
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
