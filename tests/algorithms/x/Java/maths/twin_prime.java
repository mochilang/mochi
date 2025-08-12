public class Main {

    static boolean is_prime(long n) {
        if (n < (long)(2)) {
            return false;
        }
        if (Math.floorMod(n, 2) == (long)(0)) {
            return n == (long)(2);
        }
        long i_1 = 3L;
        while ((long)((long)(i_1) * (long)(i_1)) <= n) {
            if (Math.floorMod(n, i_1) == (long)(0)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + (long)(2));
        }
        return true;
    }

    static long twin_prime(long number) {
        if (is_prime(number) && is_prime((long)(number + (long)(2)))) {
            return number + (long)(2);
        }
        return -1;
    }

    static void test_twin_prime() {
        if (twin_prime(3L) != (long)(5)) {
            throw new RuntimeException(String.valueOf("twin_prime(3) failed"));
        }
        if (twin_prime(4L) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("twin_prime(4) failed"));
        }
        if (twin_prime(5L) != (long)(7)) {
            throw new RuntimeException(String.valueOf("twin_prime(5) failed"));
        }
        if (twin_prime(17L) != (long)(19)) {
            throw new RuntimeException(String.valueOf("twin_prime(17) failed"));
        }
        if (twin_prime(0L) != (long)((-1))) {
            throw new RuntimeException(String.valueOf("twin_prime(0) failed"));
        }
    }

    static void main() {
        test_twin_prime();
        System.out.println(twin_prime(3L));
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
