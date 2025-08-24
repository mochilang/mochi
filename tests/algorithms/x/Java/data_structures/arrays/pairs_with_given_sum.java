public class Main {

    static java.math.BigInteger pairs_with_sum(java.math.BigInteger[] arr, java.math.BigInteger req_sum) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger count_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
            while (j_1.compareTo(n) < 0) {
                if (arr[(int)(((java.math.BigInteger)(i_1)).longValue())].add(arr[(int)(((java.math.BigInteger)(j_1)).longValue())]).compareTo(req_sum) == 0) {
                    count_1 = new java.math.BigInteger(String.valueOf(count_1.add(java.math.BigInteger.valueOf(1))));
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return count_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(pairs_with_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(1)})), java.math.BigInteger.valueOf(6)));
            System.out.println(pairs_with_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1)})), java.math.BigInteger.valueOf(2)));
            System.out.println(pairs_with_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(8)})), java.math.BigInteger.valueOf(7)));
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
}
