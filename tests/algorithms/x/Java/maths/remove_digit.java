public class Main {

    static int remove_digit(int num) {
        int n = num;
        if (n < 0) {
            n = -n;
        }
        int max_val = 0;
        int divisor = 1;
        while (divisor <= n) {
            int higher = n / (divisor * 10);
            int lower = Math.floorMod(n, divisor);
            int candidate = higher * divisor + lower;
            if (candidate > max_val) {
                max_val = candidate;
            }
            divisor = divisor * 10;
        }
        return max_val;
    }

    static void test_remove_digit() {
        if (remove_digit(152) != 52) {
            throw new RuntimeException(String.valueOf("remove_digit(152) failed"));
        }
        if (remove_digit(6385) != 685) {
            throw new RuntimeException(String.valueOf("remove_digit(6385) failed"));
        }
        if (remove_digit(-11) != 1) {
            throw new RuntimeException(String.valueOf("remove_digit(-11) failed"));
        }
        if (remove_digit(2222222) != 222222) {
            throw new RuntimeException(String.valueOf("remove_digit(2222222) failed"));
        }
    }

    static void main() {
        test_remove_digit();
        System.out.println(remove_digit(152));
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
