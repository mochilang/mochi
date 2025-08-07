public class Main {

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean isclose(double a, double b, double tolerance) {
        return abs_float(a - b) < tolerance;
    }

    static double focal_length(double distance_of_object, double distance_of_image) {
        if (distance_of_object == 0.0 || distance_of_image == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / distance_of_object) + (1.0 / distance_of_image));
    }

    static double object_distance(double focal_length, double distance_of_image) {
        if (distance_of_image == 0.0 || focal_length == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_image));
    }

    static double image_distance(double focal_length, double distance_of_object) {
        if (distance_of_object == 0.0 || focal_length == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_object));
    }

    static void test_focal_length() {
        double f1 = focal_length(10.0, 20.0);
        if (!(Boolean)isclose(f1, 6.66666666666666, 1e-08)) {
            throw new RuntimeException(String.valueOf("focal_length test1 failed"));
        }
        double f2 = focal_length(9.5, 6.7);
        if (!(Boolean)isclose(f2, 3.929012346, 1e-08)) {
            throw new RuntimeException(String.valueOf("focal_length test2 failed"));
        }
    }

    static void test_object_distance() {
        double u1 = object_distance(30.0, 20.0);
        if (!(Boolean)isclose(u1, -60.0, 1e-08)) {
            throw new RuntimeException(String.valueOf("object_distance test1 failed"));
        }
        double u2 = object_distance(10.5, 11.7);
        if (!(Boolean)isclose(u2, 102.375, 1e-08)) {
            throw new RuntimeException(String.valueOf("object_distance test2 failed"));
        }
    }

    static void test_image_distance() {
        double v1 = image_distance(10.0, 40.0);
        if (!(Boolean)isclose(v1, 13.33333333, 1e-08)) {
            throw new RuntimeException(String.valueOf("image_distance test1 failed"));
        }
        double v2 = image_distance(1.5, 6.7);
        if (!(Boolean)isclose(v2, 1.932692308, 1e-08)) {
            throw new RuntimeException(String.valueOf("image_distance test2 failed"));
        }
    }

    static void main() {
        test_focal_length();
        test_object_distance();
        test_image_distance();
        System.out.println(_p(focal_length(10.0, 20.0)));
        System.out.println(_p(object_distance(30.0, 20.0)));
        System.out.println(_p(image_distance(10.0, 40.0)));
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
        return String.valueOf(v);
    }
}
