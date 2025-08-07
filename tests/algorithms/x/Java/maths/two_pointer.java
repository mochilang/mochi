public class Main {

    static int[] two_pointer(int[] nums, int target) {
        int i = 0;
        int j = nums.length - 1;
        while (i < j) {
            int s = nums[i] + nums[j];
            if (s == target) {
                return new int[]{i, j};
            }
            if (s < target) {
                i = i + 1;
            } else {
                j = j - 1;
            }
        }
        return new int[]{};
    }

    static void test_two_pointer() {
        if (two_pointer(((int[])(new int[]{2, 7, 11, 15})), 9) != new int[]{0, 1}) {
            throw new RuntimeException(String.valueOf("case1"));
        }
        if (two_pointer(((int[])(new int[]{2, 7, 11, 15})), 17) != new int[]{0, 3}) {
            throw new RuntimeException(String.valueOf("case2"));
        }
        if (two_pointer(((int[])(new int[]{2, 7, 11, 15})), 18) != new int[]{1, 2}) {
            throw new RuntimeException(String.valueOf("case3"));
        }
        if (two_pointer(((int[])(new int[]{2, 7, 11, 15})), 26) != new int[]{2, 3}) {
            throw new RuntimeException(String.valueOf("case4"));
        }
        if (two_pointer(((int[])(new int[]{1, 3, 3})), 6) != new int[]{1, 2}) {
            throw new RuntimeException(String.valueOf("case5"));
        }
        if (two_pointer(((int[])(new int[]{2, 7, 11, 15})), 8).length != 0) {
            throw new RuntimeException(String.valueOf("case6"));
        }
        if (two_pointer(((int[])(new int[]{0, 3, 6, 9, 12, 15, 18, 21, 24, 27})), 19).length != 0) {
            throw new RuntimeException(String.valueOf("case7"));
        }
        if (two_pointer(((int[])(new int[]{1, 2, 3})), 6).length != 0) {
            throw new RuntimeException(String.valueOf("case8"));
        }
    }

    static void main() {
        test_two_pointer();
        System.out.println(two_pointer(((int[])(new int[]{2, 7, 11, 15})), 9));
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
}
