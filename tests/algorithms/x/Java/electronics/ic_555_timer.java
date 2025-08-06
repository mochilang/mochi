public class Main {

    static double astable_frequency(double resistance_1, double resistance_2, double capacitance) {
        if (resistance_1 <= 0.0 || resistance_2 <= 0.0 || capacitance <= 0.0) {
            throw new RuntimeException(String.valueOf("All values must be positive"));
        }
        return (1.44 / ((resistance_1 + 2.0 * resistance_2) * capacitance)) * 1000000.0;
    }

    static double astable_duty_cycle(double resistance_1, double resistance_2) {
        if (resistance_1 <= 0.0 || resistance_2 <= 0.0) {
            throw new RuntimeException(String.valueOf("All values must be positive"));
        }
        return (resistance_1 + resistance_2) / (resistance_1 + 2.0 * resistance_2) * 100.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(astable_frequency(45.0, 45.0, 7.0));
            System.out.println(astable_duty_cycle(45.0, 45.0));
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
