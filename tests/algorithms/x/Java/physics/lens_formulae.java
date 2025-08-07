public class Main {

    static double focal_length_of_lens(double object_distance_from_lens, double image_distance_from_lens) {
        if (object_distance_from_lens == 0.0 || image_distance_from_lens == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / object_distance_from_lens));
    }

    static double object_distance(double focal_length_of_lens, double image_distance_from_lens) {
        if (image_distance_from_lens == 0.0 || focal_length_of_lens == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / focal_length_of_lens));
    }

    static double image_distance(double focal_length_of_lens, double object_distance_from_lens) {
        if (object_distance_from_lens == 0.0 || focal_length_of_lens == 0.0) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return 1.0 / ((1.0 / object_distance_from_lens) + (1.0 / focal_length_of_lens));
    }
    public static void main(String[] args) {
        System.out.println(_p(focal_length_of_lens(10.0, 4.0)));
        System.out.println(_p(focal_length_of_lens(2.7, 5.8)));
        System.out.println(_p(object_distance(10.0, 40.0)));
        System.out.println(_p(object_distance(6.2, 1.5)));
        System.out.println(_p(image_distance(50.0, 40.0)));
        System.out.println(_p(image_distance(5.3, 7.9)));
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
