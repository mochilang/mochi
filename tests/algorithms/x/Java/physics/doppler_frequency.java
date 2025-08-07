public class Main {

    static double doppler_effect(double org_freq, double wave_vel, double obs_vel, double src_vel) {
        if (wave_vel == src_vel) {
            throw new RuntimeException(String.valueOf("division by zero implies vs=v and observer in front of the source"));
        }
        double doppler_freq = (org_freq * (wave_vel + obs_vel)) / (wave_vel - src_vel);
        if (doppler_freq <= 0.0) {
            throw new RuntimeException(String.valueOf("non-positive frequency implies vs>v or v0>v (in the opposite direction)"));
        }
        return doppler_freq;
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean almost_equal(double a, double b, double tol) {
        return absf(a - b) <= tol;
    }

    static void test_doppler_effect() {
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, 10.0, 0.0), 103.03030303030303, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 1 failed"));
        }
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, -10.0, 0.0), 96.96969696969697, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 2 failed"));
        }
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, 0.0, 10.0), 103.125, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 3 failed"));
        }
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, 0.0, -10.0), 97.05882352941177, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 4 failed"));
        }
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, 10.0, 10.0), 106.25, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 5 failed"));
        }
        if (!(Boolean)almost_equal(doppler_effect(100.0, 330.0, -10.0, -10.0), 94.11764705882354, 1e-07)) {
            throw new RuntimeException(String.valueOf("test 6 failed"));
        }
    }

    static void main() {
        test_doppler_effect();
        System.out.println(doppler_effect(100.0, 330.0, 10.0, 0.0));
    }
    public static void main(String[] args) {
        main();
    }
}
