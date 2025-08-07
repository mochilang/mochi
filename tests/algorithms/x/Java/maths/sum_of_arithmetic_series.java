public class Main {

    static int sum_of_series(int first_term, int common_diff, int num_of_terms) {
        int total = Math.floorDiv(num_of_terms * (2 * first_term + (num_of_terms - 1) * common_diff), 2);
        return total;
    }

    static void test_sum_of_series() {
        if (sum_of_series(1, 1, 10) != 55) {
            throw new RuntimeException(String.valueOf("sum_of_series(1, 1, 10) failed"));
        }
        if (sum_of_series(1, 10, 100) != 49600) {
            throw new RuntimeException(String.valueOf("sum_of_series(1, 10, 100) failed"));
        }
    }

    static void main() {
        test_sum_of_series();
        System.out.println(sum_of_series(1, 1, 10));
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
