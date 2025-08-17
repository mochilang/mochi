public class Main {

    static long factorial(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        long value_1 = 1L;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(n)) {
            value_1 = (long)((long)(value_1) * (long)(i_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return value_1;
    }

    static long factorial_recursive(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        if ((long)(n) <= 1L) {
            return 1;
        }
        return (long)(n) * (long)(factorial_recursive((long)((long)(n) - 1L)));
    }

    static void test_zero() {
        if ((long)(factorial(0L)) != 1L) {
            throw new RuntimeException(String.valueOf("factorial(0) failed"));
        }
        if ((long)(factorial_recursive(0L)) != 1L) {
            throw new RuntimeException(String.valueOf("factorial_recursive(0) failed"));
        }
    }

    static void test_positive_integers() {
        if ((long)(factorial(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("factorial(1) failed"));
        }
        if ((long)(factorial_recursive(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("factorial_recursive(1) failed"));
        }
        if ((long)(factorial(5L)) != 120L) {
            throw new RuntimeException(String.valueOf("factorial(5) failed"));
        }
        if ((long)(factorial_recursive(5L)) != 120L) {
            throw new RuntimeException(String.valueOf("factorial_recursive(5) failed"));
        }
        if ((long)(factorial(7L)) != 5040L) {
            throw new RuntimeException(String.valueOf("factorial(7) failed"));
        }
        if ((long)(factorial_recursive(7L)) != 5040L) {
            throw new RuntimeException(String.valueOf("factorial_recursive(7) failed"));
        }
    }

    static void test_large_number() {
        if ((long)(factorial(10L)) != 3628800L) {
            throw new RuntimeException(String.valueOf("factorial(10) failed"));
        }
        if ((long)(factorial_recursive(10L)) != 3628800L) {
            throw new RuntimeException(String.valueOf("factorial_recursive(10) failed"));
        }
    }

    static void run_tests() {
        test_zero();
        test_positive_integers();
        test_large_number();
    }

    static void main() {
        run_tests();
        System.out.println(factorial(6L));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
