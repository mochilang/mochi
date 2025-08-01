public class Main {
    static java.math.BigInteger fib(int n) {
        if (n < 2) {
            return new java.math.BigInteger(String.valueOf(n));
        }
        java.math.BigInteger a = java.math.BigInteger.valueOf(0);
        java.math.BigInteger b = java.math.BigInteger.valueOf(1);
        int i = n;
        i = i - 1;
        while (i > 0) {
            java.math.BigInteger tmp = a.add(b);
            a = b;
            b = new java.math.BigInteger(String.valueOf(tmp));
            i = i - 1;
        }
        return b;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
