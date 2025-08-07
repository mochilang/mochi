public class Main {
    static double G;

    static double archimedes_principle(double fluid_density, double volume, double gravity) {
        if (fluid_density <= 0.0) {
            throw new RuntimeException(String.valueOf("Impossible fluid density"));
        }
        if (volume <= 0.0) {
            throw new RuntimeException(String.valueOf("Impossible object volume"));
        }
        if (gravity < 0.0) {
            throw new RuntimeException(String.valueOf("Impossible gravity"));
        }
        return fluid_density * volume * gravity;
    }

    static double archimedes_principle_default(double fluid_density, double volume) {
        double res = archimedes_principle(fluid_density, volume, G);
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            G = 9.80665;
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
