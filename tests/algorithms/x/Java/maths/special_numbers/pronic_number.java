public class Main {

    static long int_sqrt(long n) {
        long r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static boolean is_pronic(long n) {
        if (n < 0) {
            return false;
        }
        if (Math.floorMod(n, 2) != 0) {
            return false;
        }
        long root = int_sqrt(n);
        return n == root * (root + 1);
    }

    static void test_is_pronic() {
        if (((Boolean)(is_pronic(-1)))) {
            throw new RuntimeException(String.valueOf("-1 should not be pronic"));
        }
        if (!(Boolean)is_pronic(0)) {
            throw new RuntimeException(String.valueOf("0 should be pronic"));
        }
        if (!(Boolean)is_pronic(2)) {
            throw new RuntimeException(String.valueOf("2 should be pronic"));
        }
        if (((Boolean)(is_pronic(5)))) {
            throw new RuntimeException(String.valueOf("5 should not be pronic"));
        }
        if (!(Boolean)is_pronic(6)) {
            throw new RuntimeException(String.valueOf("6 should be pronic"));
        }
        if (((Boolean)(is_pronic(8)))) {
            throw new RuntimeException(String.valueOf("8 should not be pronic"));
        }
        if (!(Boolean)is_pronic(30)) {
            throw new RuntimeException(String.valueOf("30 should be pronic"));
        }
        if (((Boolean)(is_pronic(32)))) {
            throw new RuntimeException(String.valueOf("32 should not be pronic"));
        }
        if (!(Boolean)is_pronic(2147441940)) {
            throw new RuntimeException(String.valueOf("2147441940 should be pronic"));
        }
    }

    static void main() {
        test_is_pronic();
        System.out.println(is_pronic(56));
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
    static long _nowSeed;
    static long _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Long.parseLong(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (_nowSeed * 1664525L + 1013904223) % 2147483647L;
            return _nowSeed;
        }
        return System.nanoTime() / 1000;
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
