public class Main {

    static int triangular_number(int position) {
        if (position < 0) {
            throw new RuntimeException(String.valueOf("position must be non-negative"));
        }
        return Math.floorDiv(position * (position + 1), 2);
    }

    static void test_triangular_number() {
        if (triangular_number(1) != 1) {
            throw new RuntimeException(String.valueOf("triangular_number(1) failed"));
        }
        if (triangular_number(3) != 6) {
            throw new RuntimeException(String.valueOf("triangular_number(3) failed"));
        }
    }

    static void main() {
        test_triangular_number();
        System.out.println(triangular_number(10));
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
