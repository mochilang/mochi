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

    static void test_factorial() {
        int i_1 = 0;
        while (i_1 <= 10) {
            if (factorial(i_1) != factorial_recursive(i_1)) {
                throw new RuntimeException(String.valueOf("mismatch between factorial and factorial_recursive"));
            }
            i_1 = i_1 + 1;
        }
        if (factorial(6) != 720) {
            throw new RuntimeException(String.valueOf("factorial(6) should be 720"));
        }
    }

    static void main() {
        test_factorial();
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
