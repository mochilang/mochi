public class Main {
    static double G = (double)(9.80665);

    static double potential_energy(double mass, double height) {
        if ((double)(mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The mass of a body cannot be negative"));
        }
        if ((double)(height) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The height above the ground cannot be negative"));
        }
        return (double)((double)(mass) * (double)(G)) * (double)(height);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(potential_energy((double)(10.0), (double)(10.0)));
            System.out.println(potential_energy((double)(10.0), (double)(5.0)));
            System.out.println(potential_energy((double)(2.0), (double)(8.0)));
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
