public class Main {

    static double kinetic_energy(double mass, double velocity) {
        if ((double)(mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The mass of a body cannot be negative"));
        }
        double v_1 = (double)(velocity);
        if ((double)(v_1) < (double)(0.0)) {
            v_1 = (double)(-v_1);
        }
        return (double)((double)((double)(0.5) * (double)(mass)) * (double)(v_1)) * (double)(v_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(kinetic_energy((double)(10.0), (double)(10.0)));
            System.out.println(kinetic_energy((double)(0.0), (double)(10.0)));
            System.out.println(kinetic_energy((double)(10.0), (double)(0.0)));
            System.out.println(kinetic_energy((double)(20.0), (double)(-20.0)));
            System.out.println(kinetic_energy((double)(0.0), (double)(0.0)));
            System.out.println(kinetic_energy((double)(2.0), (double)(2.0)));
            System.out.println(kinetic_energy((double)(100.0), (double)(100.0)));
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
