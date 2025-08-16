public class Main {

    static double abs_float(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static boolean isclose(double a, double b, double tolerance) {
        return (double)(abs_float((double)((double)(a) - (double)(b)))) < (double)(tolerance);
    }

    static double focal_length(double distance_of_object, double distance_of_image) {
        if ((double)(distance_of_object) == (double)(0.0) || (double)(distance_of_image) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)(1.0) / (double)(((double)(((double)(1.0) / (double)(distance_of_object))) + (double)(((double)(1.0) / (double)(distance_of_image)))));
    }

    static double object_distance(double focal_length, double distance_of_image) {
        if ((double)(distance_of_image) == (double)(0.0) || (double)(focal_length) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)(1.0) / (double)(((double)(((double)(1.0) / (double)(focal_length))) - (double)(((double)(1.0) / (double)(distance_of_image)))));
    }

    static double image_distance(double focal_length, double distance_of_object) {
        if ((double)(distance_of_object) == (double)(0.0) || (double)(focal_length) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)(1.0) / (double)(((double)(((double)(1.0) / (double)(focal_length))) - (double)(((double)(1.0) / (double)(distance_of_object)))));
    }

    static void test_focal_length() {
        double f1 = (double)(focal_length((double)(10.0), (double)(20.0)));
        if (!(Boolean)isclose((double)(f1), (double)(6.66666666666666), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("focal_length test1 failed"));
        }
        double f2_1 = (double)(focal_length((double)(9.5), (double)(6.7)));
        if (!(Boolean)isclose((double)(f2_1), (double)(3.929012346), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("focal_length test2 failed"));
        }
    }

    static void test_object_distance() {
        double u1 = (double)(object_distance((double)(30.0), (double)(20.0)));
        if (!(Boolean)isclose((double)(u1), (double)(-60.0), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("object_distance test1 failed"));
        }
        double u2_1 = (double)(object_distance((double)(10.5), (double)(11.7)));
        if (!(Boolean)isclose((double)(u2_1), (double)(102.375), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("object_distance test2 failed"));
        }
    }

    static void test_image_distance() {
        double v1 = (double)(image_distance((double)(10.0), (double)(40.0)));
        if (!(Boolean)isclose((double)(v1), (double)(13.33333333), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("image_distance test1 failed"));
        }
        double v2_1 = (double)(image_distance((double)(1.5), (double)(6.7)));
        if (!(Boolean)isclose((double)(v2_1), (double)(1.932692308), (double)(1e-08))) {
            throw new RuntimeException(String.valueOf("image_distance test2 failed"));
        }
    }

    static void main() {
        test_focal_length();
        test_object_distance();
        test_image_distance();
        System.out.println(_p(focal_length((double)(10.0), (double)(20.0))));
        System.out.println(_p(object_distance((double)(30.0), (double)(20.0))));
        System.out.println(_p(image_distance((double)(10.0), (double)(40.0))));
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
