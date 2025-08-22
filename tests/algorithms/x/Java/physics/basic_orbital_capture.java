public class Main {
    static double G = (double)(6.6743e-11);
    static double C = (double)(299792458.0);
    static double PI = (double)(3.141592653589793);

    static double pow10(long n) {
        double result = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            result = (double)((double)(result) * (double)(10.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_3 = 0L;
        while ((long)(i_3) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return guess_1;
    }

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double capture_radii(double target_body_radius, double target_body_mass, double projectile_velocity) {
        if ((double)(target_body_mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Mass cannot be less than 0"));
        }
        if ((double)(target_body_radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Radius cannot be less than 0"));
        }
        if ((double)(projectile_velocity) > (double)(C)) {
            throw new RuntimeException(String.valueOf("Cannot go beyond speed of light"));
        }
        double escape_velocity_squared_1 = (double)((double)(((double)((double)(2.0) * (double)(G)) * (double)(target_body_mass))) / (double)(target_body_radius));
        double denom_1 = (double)((double)(projectile_velocity) * (double)(projectile_velocity));
        double capture_radius_1 = (double)((double)(target_body_radius) * (double)(sqrt((double)((double)(1.0) + (double)((double)(escape_velocity_squared_1) / (double)(denom_1))))));
        return capture_radius_1;
    }

    static double capture_area(double capture_radius) {
        if ((double)(capture_radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Cannot have a capture radius less than 0"));
        }
        double sigma_1 = (double)((double)((double)(PI) * (double)(capture_radius)) * (double)(capture_radius));
        return sigma_1;
    }

    static void run_tests() {
        double r = (double)(capture_radii((double)((double)(6.957) * (double)(pow10(8L))), (double)((double)(1.99) * (double)(pow10(30L))), (double)(25000.0)));
        if (Math.abs((double)(r) - (double)((double)(1.720959069143714) * (double)(pow10(10L)))) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("capture_radii failed"));
        }
        double a_1 = (double)(capture_area((double)(r)));
        if (Math.abs((double)(a_1) - (double)((double)(9.304455331801812) * (double)(pow10(20L)))) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("capture_area failed"));
        }
    }

    static void main() {
        run_tests();
        double r_2 = (double)(capture_radii((double)((double)(6.957) * (double)(pow10(8L))), (double)((double)(1.99) * (double)(pow10(30L))), (double)(25000.0)));
        System.out.println(_p(r_2));
        System.out.println(_p(capture_area((double)(r_2))));
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
