public class Main {
    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);
    static double g = (double)(9.80665);
    static double v0 = (double)(25.0);
    static double angle = (double)(20.0);

    static double _mod(double x, double m) {
        return (double)(x) - (double)((double)((((Number)(((Number)((double)(x) / (double)(m))).intValue())).doubleValue())) * (double)(m));
    }

    static double sin(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y3_1 = (double)((double)(y2_1) * (double)(y));
        double y5_1 = (double)((double)(y3_1) * (double)(y2_1));
        double y7_1 = (double)((double)(y5_1) * (double)(y2_1));
        return (double)((double)((double)(y) - (double)((double)(y3_1) / (double)(6.0))) + (double)((double)(y5_1) / (double)(120.0))) - (double)((double)(y7_1) / (double)(5040.0));
    }

    static double deg_to_rad(double deg) {
        return (double)((double)(deg) * (double)(PI)) / (double)(180.0);
    }

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double result = (double)(1.0);
        long i_2 = 0L;
        while ((long)(i_2) < (long)(n)) {
            result = (double)((double)(result) * (double)(10.0));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return result;
    }

    static double round(double x, long n) {
        double m = (double)(pow10((long)(n)));
        double y_2 = ((Number)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)))).doubleValue();
        return (double)(y_2) / (double)(m);
    }

    static void check_args(double init_velocity, double angle) {
        if ((double)(angle) > (double)(90.0) || (double)(angle) < (double)(1.0)) {
            throw new RuntimeException(String.valueOf("Invalid angle. Range is 1-90 degrees."));
        }
        if ((double)(init_velocity) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid velocity. Should be a positive number."));
        }
    }

    static double horizontal_distance(double init_velocity, double angle) {
        check_args((double)(init_velocity), (double)(angle));
        double radians_1 = (double)(deg_to_rad((double)((double)(2.0) * (double)(angle))));
        return round((double)((double)(((double)((double)(init_velocity) * (double)(init_velocity)) * (double)(sin((double)(radians_1))))) / (double)(g)), 2L);
    }

    static double max_height(double init_velocity, double angle) {
        check_args((double)(init_velocity), (double)(angle));
        double radians_3 = (double)(deg_to_rad((double)(angle)));
        double s_1 = (double)(sin((double)(radians_3)));
        return round((double)((double)(((double)((double)((double)(init_velocity) * (double)(init_velocity)) * (double)(s_1)) * (double)(s_1))) / (double)(((double)(2.0) * (double)(g)))), 2L);
    }

    static double total_time(double init_velocity, double angle) {
        check_args((double)(init_velocity), (double)(angle));
        double radians_5 = (double)(deg_to_rad((double)(angle)));
        return round((double)((double)(((double)((double)(2.0) * (double)(init_velocity)) * (double)(sin((double)(radians_5))))) / (double)(g)), 2L);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(horizontal_distance((double)(v0), (double)(angle)));
            System.out.println(max_height((double)(v0), (double)(angle)));
            System.out.println(total_time((double)(v0), (double)(angle)));
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
