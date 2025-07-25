public class Main {

    static int pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int ackermann2(int m, int n) {
        if (m == 0) {
            return n + 1;
        }
        if (m == 1) {
            return n + 2;
        }
        if (m == 2) {
            return 2 * n + 3;
        }
        if (m == 3) {
            return 8 * pow(2, n) - 3;
        }
        if (n == 0) {
            return ackermann2(m - 1, 1);
        }
        return ackermann2(m - 1, ackermann2(m, n - 1));
    }

    static void main() {
        System.out.println("A(0, 0) = " + String.valueOf(ackermann2(0, 0)));
        System.out.println("A(1, 2) = " + String.valueOf(ackermann2(1, 2)));
        System.out.println("A(2, 4) = " + String.valueOf(ackermann2(2, 4)));
        System.out.println("A(3, 4) = " + String.valueOf(ackermann2(3, 4)));
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
