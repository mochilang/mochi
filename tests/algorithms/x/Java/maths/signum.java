public class Main {

    static int signum(double num) {
        if (num < 0.0) {
            return -1;
        }
        if (num > 0.0) {
            return 1;
        }
        return 0;
    }

    static void test_signum() {
        if (signum(5.0) != 1) {
            throw new RuntimeException(String.valueOf("signum(5) failed"));
        }
        if (signum(-5.0) != (-1)) {
            throw new RuntimeException(String.valueOf("signum(-5) failed"));
        }
        if (signum(0.0) != 0) {
            throw new RuntimeException(String.valueOf("signum(0) failed"));
        }
        if (signum(10.5) != 1) {
            throw new RuntimeException(String.valueOf("signum(10.5) failed"));
        }
        if (signum(-10.5) != (-1)) {
            throw new RuntimeException(String.valueOf("signum(-10.5) failed"));
        }
        if (signum(1e-06) != 1) {
            throw new RuntimeException(String.valueOf("signum(1e-6) failed"));
        }
        if (signum(-1e-06) != (-1)) {
            throw new RuntimeException(String.valueOf("signum(-1e-6) failed"));
        }
        if (signum(123456789.0) != 1) {
            throw new RuntimeException(String.valueOf("signum(123456789) failed"));
        }
        if (signum(-123456789.0) != (-1)) {
            throw new RuntimeException(String.valueOf("signum(-123456789) failed"));
        }
    }

    static void main() {
        test_signum();
        System.out.println(signum(12.0));
        System.out.println(signum(-12.0));
        System.out.println(signum(0.0));
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
