public class Main {

    static int double_factorial_recursive(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("double_factorial_recursive() not defined for negative values"));
        }
        if (n <= 1) {
            return 1;
        }
        return n * double_factorial_recursive(n - 2);
    }

    static int double_factorial_iterative(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("double_factorial_iterative() not defined for negative values"));
        }
        int result = 1;
        int i = n;
        while (i > 0) {
            result = result * i;
            i = i - 2;
        }
        return result;
    }

    static void test_double_factorial() {
        if (double_factorial_recursive(0) != 1) {
            throw new RuntimeException(String.valueOf("0!! recursive failed"));
        }
        if (double_factorial_iterative(0) != 1) {
            throw new RuntimeException(String.valueOf("0!! iterative failed"));
        }
        if (double_factorial_recursive(1) != 1) {
            throw new RuntimeException(String.valueOf("1!! recursive failed"));
        }
        if (double_factorial_iterative(1) != 1) {
            throw new RuntimeException(String.valueOf("1!! iterative failed"));
        }
        if (double_factorial_recursive(5) != 15) {
            throw new RuntimeException(String.valueOf("5!! recursive failed"));
        }
        if (double_factorial_iterative(5) != 15) {
            throw new RuntimeException(String.valueOf("5!! iterative failed"));
        }
        if (double_factorial_recursive(6) != 48) {
            throw new RuntimeException(String.valueOf("6!! recursive failed"));
        }
        if (double_factorial_iterative(6) != 48) {
            throw new RuntimeException(String.valueOf("6!! iterative failed"));
        }
        int n = 0;
        while (n <= 10) {
            if (double_factorial_recursive(n) != double_factorial_iterative(n)) {
                throw new RuntimeException(String.valueOf("double factorial mismatch"));
            }
            n = n + 1;
        }
    }

    static void main() {
        test_double_factorial();
        System.out.println(double_factorial_iterative(10));
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
