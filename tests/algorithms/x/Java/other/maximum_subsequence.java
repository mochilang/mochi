public class Main {

    static java.math.BigInteger max_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) >= 0) {
            return new java.math.BigInteger(String.valueOf(a));
        } else {
            return new java.math.BigInteger(String.valueOf(b));
        }
    }

    static java.math.BigInteger max_subsequence_sum(java.math.BigInteger[] nums) {
        if (new java.math.BigInteger(String.valueOf(nums.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("input sequence should not be empty"));
        }
        java.math.BigInteger ans_1 = new java.math.BigInteger(String.valueOf(nums[_idx((nums).length, 0L)]));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
            java.math.BigInteger num_1 = new java.math.BigInteger(String.valueOf(nums[_idx((nums).length, ((java.math.BigInteger)(i_1)).longValue())]));
            java.math.BigInteger extended_1 = new java.math.BigInteger(String.valueOf(ans_1.add(num_1)));
            ans_1 = new java.math.BigInteger(String.valueOf(max_int(new java.math.BigInteger(String.valueOf(max_int(new java.math.BigInteger(String.valueOf(ans_1)), new java.math.BigInteger(String.valueOf(extended_1))))), new java.math.BigInteger(String.valueOf(num_1)))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(ans_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(max_subsequence_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate()))}))));
            System.out.println(max_subsequence_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(6)).negate()))}))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
