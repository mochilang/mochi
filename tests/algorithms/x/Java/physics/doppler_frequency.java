public class Main {

    static double doppler_effect(double org_freq, double wave_vel, double obs_vel, double src_vel) {
        if ((double)(wave_vel) == (double)(src_vel)) {
            throw new RuntimeException(String.valueOf("division by zero implies vs=v and observer in front of the source"));
        }
        double doppler_freq_1 = (double)((double)(((double)(org_freq) * (double)(((double)(wave_vel) + (double)(obs_vel))))) / (double)(((double)(wave_vel) - (double)(src_vel))));
        if ((double)(doppler_freq_1) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("non-positive frequency implies vs>v or v0>v (in the opposite direction)"));
        }
        return doppler_freq_1;
    }

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static boolean almost_equal(double a, double b, double tol) {
        return (double)(absf((double)((double)(a) - (double)(b)))) <= (double)(tol);
    }

    static void test_doppler_effect() {
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(10.0), (double)(0.0))), (double)(103.03030303030303), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 1 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(-10.0), (double)(0.0))), (double)(96.96969696969697), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 2 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(0.0), (double)(10.0))), (double)(103.125), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 3 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(0.0), (double)(-10.0))), (double)(97.05882352941177), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 4 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(10.0), (double)(10.0))), (double)(106.25), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 5 failed"));
        }
        if (!(Boolean)almost_equal((double)(doppler_effect((double)(100.0), (double)(330.0), (double)(-10.0), (double)(-10.0))), (double)(94.11764705882354), (double)(1e-07))) {
            throw new RuntimeException(String.valueOf("test 6 failed"));
        }
    }

    static void main() {
        test_doppler_effect();
        System.out.println(doppler_effect((double)(100.0), (double)(330.0), (double)(10.0), (double)(0.0)));
    }
    public static void main(String[] args) {
        main();
    }
}
