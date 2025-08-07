public class Main {

    static int ugly_numbers(int n) {
        if (n <= 0) {
            return 1;
        }
        int[] ugly_nums = ((int[])(new int[]{}));
        ugly_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ugly_nums), java.util.stream.IntStream.of(1)).toArray()));
        int i2 = 0;
        int i3 = 0;
        int i5 = 0;
        int next_2 = 2;
        int next_3 = 3;
        int next_5 = 5;
        int count = 1;
        while (count < n) {
            int next_num = next_2 < next_3 ? next_2 < next_5 ? next_2 : next_5 : next_3 < next_5 ? next_3 : next_5;
            ugly_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ugly_nums), java.util.stream.IntStream.of(next_num)).toArray()));
            if (next_num == next_2) {
                i2 = i2 + 1;
                next_2 = ugly_nums[i2] * 2;
            }
            if (next_num == next_3) {
                i3 = i3 + 1;
                next_3 = ugly_nums[i3] * 3;
            }
            if (next_num == next_5) {
                i5 = i5 + 1;
                next_5 = ugly_nums[i5] * 5;
            }
            count = count + 1;
        }
        return ugly_nums[ugly_nums.length - 1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(ugly_numbers(100)));
            System.out.println(_p(ugly_numbers(0)));
            System.out.println(_p(ugly_numbers(20)));
            System.out.println(_p(ugly_numbers(-5)));
            System.out.println(_p(ugly_numbers(200)));
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
