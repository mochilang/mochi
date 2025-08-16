public class Main {

    static long max_int(long a, long b) {
        if ((long)(a) >= (long)(b)) {
            return a;
        } else {
            return b;
        }
    }

    static long max_subsequence_sum(long[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("input sequence should not be empty"));
        }
        long ans_1 = (long)(nums[(int)((long)(0))]);
        long i_1 = 1L;
        while ((long)(i_1) < (long)(nums.length)) {
            long num_1 = (long)(nums[(int)((long)(i_1))]);
            long extended_1 = (long)((long)(ans_1) + (long)(num_1));
            ans_1 = (long)(max_int((long)(max_int((long)(ans_1), (long)(extended_1))), (long)(num_1)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return ans_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(max_subsequence_sum(((long[])(new long[]{1, 2, 3, 4, -2}))));
            System.out.println(max_subsequence_sum(((long[])(new long[]{-2, -3, -1, -4, -6}))));
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
