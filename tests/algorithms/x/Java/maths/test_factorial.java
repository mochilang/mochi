public class Main {

    static int factorial(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        int value = 1;
        int i = 1;
        while (i <= n) {
            value = value * i;
            i = i + 1;
        }
        return value;
    }

    static int factorial_recursive(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        if (n <= 1) {
            return 1;
        }
        return n * factorial_recursive(n - 1);
    }

    static void test_zero() {
        if (factorial(0) != 1) {
            throw new RuntimeException(String.valueOf("factorial(0) failed"));
        }
        if (factorial_recursive(0) != 1) {
            throw new RuntimeException(String.valueOf("factorial_recursive(0) failed"));
        }
    }

    static void test_positive_integers() {
        if (factorial(1) != 1) {
            throw new RuntimeException(String.valueOf("factorial(1) failed"));
        }
        if (factorial_recursive(1) != 1) {
            throw new RuntimeException(String.valueOf("factorial_recursive(1) failed"));
        }
        if (factorial(5) != 120) {
            throw new RuntimeException(String.valueOf("factorial(5) failed"));
        }
        if (factorial_recursive(5) != 120) {
            throw new RuntimeException(String.valueOf("factorial_recursive(5) failed"));
        }
        if (factorial(7) != 5040) {
            throw new RuntimeException(String.valueOf("factorial(7) failed"));
        }
        if (factorial_recursive(7) != 5040) {
            throw new RuntimeException(String.valueOf("factorial_recursive(7) failed"));
        }
    }

    static void test_large_number() {
        if (factorial(10) != 3628800) {
            throw new RuntimeException(String.valueOf("factorial(10) failed"));
        }
        if (factorial_recursive(10) != 3628800) {
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
        System.out.println(factorial(6));
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
