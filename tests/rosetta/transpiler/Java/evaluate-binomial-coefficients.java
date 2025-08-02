public class Main {

    static java.math.BigInteger binom(int n, int k) {
        if (k < 0 || k > n) {
            return java.math.BigInteger.valueOf(0);
        }
        int kk = k;
        if (kk > n - kk) {
            kk = n - kk;
        }
        java.math.BigInteger res = java.math.BigInteger.valueOf(1);
        int i = 0;
        while (i < kk) {
            res = res.multiply((new java.math.BigInteger(String.valueOf((n - i)))));
            i = i + 1;
            res = res.divide((new java.math.BigInteger(String.valueOf(i))));
        }
        return res;
    }

    static void main() {
        System.out.println(_p(binom(5, 3)));
        System.out.println(_p(binom(60, 30)));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
