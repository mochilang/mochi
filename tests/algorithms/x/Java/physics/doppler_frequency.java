public class Main {

    static double doppler_effect(double org_freq, double wave_vel, double obs_vel, double src_vel) {
        if ((double)(wave_vel) == (double)(src_vel)) {
            throw new RuntimeException(String.valueOf("division by zero implies vs=v and observer in front of the source"));
        }
        double doppler_freq_1 = ((double)(org_freq) * ((double)(wave_vel) + (double)(obs_vel))) / ((double)(wave_vel) - (double)(src_vel));
        if (doppler_freq_1 <= 0.0) {
            throw new RuntimeException(String.valueOf("non-positive frequency implies vs>v or v0>v (in the opposite direction)"));
        }
        return doppler_freq_1;
    }

    static double absf(double x) {
        if ((double)(x) < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean almost_equal(double a, double b, double tol) {
        return (double)(absf((double)(a) - (double)(b))) <= (double)(tol);
    }

    static void test_doppler_effect() {
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, 10.0, 0.0)), 103.03030303030303, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 1 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, -10.0, 0.0)), 96.96969696969697, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 2 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, 0.0, 10.0)), 103.125, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 3 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, 0.0, -10.0)), 97.05882352941177, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 4 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, 10.0, 10.0)), 106.25, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 5 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect(100.0, 330.0, -10.0, -10.0)), 94.11764705882354, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 6 failed"));
        }
    }

    static void main() {
        test_doppler_effect();
        System.out.println(doppler_effect(100.0, 330.0, 10.0, 0.0));
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
