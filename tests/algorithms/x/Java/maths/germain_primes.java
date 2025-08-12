public class Main {

    static boolean is_prime(long n) {
        if (n <= 1) {
            return false;
        }
        if (n <= 3) {
            return true;
        }
        if (Math.floorMod(n, 2) == 0) {
            return false;
        }
        long i_1 = 3;
        while (i_1 * i_1 <= n) {
            if (Math.floorMod(n, i_1) == 0) {
                return false;
            }
            i_1 = i_1 + 2;
        }
        return true;
    }

    static boolean is_germain_prime(long number) {
        if (number < 1) {
            throw new RuntimeException(String.valueOf("Input value must be a positive integer"));
        }
        return ((Boolean)(is_prime(number))) && ((Boolean)(is_prime(2 * number + 1)));
    }

    static boolean is_safe_prime(long number) {
        if (number < 1) {
            throw new RuntimeException(String.valueOf("Input value must be a positive integer"));
        }
        if (Math.floorMod((number - 1), 2) != 0) {
            return false;
        }
        return ((Boolean)(is_prime(number))) && ((Boolean)(is_prime(Math.floorDiv((number - 1), 2))));
    }

    static void test_is_germain_prime() {
        if (!(Boolean)is_germain_prime(3)) {
            throw new RuntimeException(String.valueOf("is_germain_prime(3) failed"));
        }
        if (!(Boolean)is_germain_prime(11)) {
            throw new RuntimeException(String.valueOf("is_germain_prime(11) failed"));
        }
        if (((Boolean)(is_germain_prime(4)))) {
            throw new RuntimeException(String.valueOf("is_germain_prime(4) failed"));
        }
        if (!(Boolean)is_germain_prime(23)) {
            throw new RuntimeException(String.valueOf("is_germain_prime(23) failed"));
        }
        if (((Boolean)(is_germain_prime(13)))) {
            throw new RuntimeException(String.valueOf("is_germain_prime(13) failed"));
        }
        if (((Boolean)(is_germain_prime(20)))) {
            throw new RuntimeException(String.valueOf("is_germain_prime(20) failed"));
        }
    }

    static void test_is_safe_prime() {
        if (!(Boolean)is_safe_prime(5)) {
            throw new RuntimeException(String.valueOf("is_safe_prime(5) failed"));
        }
        if (!(Boolean)is_safe_prime(11)) {
            throw new RuntimeException(String.valueOf("is_safe_prime(11) failed"));
        }
        if (((Boolean)(is_safe_prime(1)))) {
            throw new RuntimeException(String.valueOf("is_safe_prime(1) failed"));
        }
        if (((Boolean)(is_safe_prime(2)))) {
            throw new RuntimeException(String.valueOf("is_safe_prime(2) failed"));
        }
        if (((Boolean)(is_safe_prime(3)))) {
            throw new RuntimeException(String.valueOf("is_safe_prime(3) failed"));
        }
        if (!(Boolean)is_safe_prime(47)) {
            throw new RuntimeException(String.valueOf("is_safe_prime(47) failed"));
        }
    }

    static void main() {
        test_is_germain_prime();
        test_is_safe_prime();
        System.out.println(is_germain_prime(23));
        System.out.println(is_safe_prime(47));
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
