public class Main {
    static String[] units;
    static double[] speed_chart;
    static double[] speed_chart_inverse;

    static int index_of(String[] arr, String value) {
        int i = 0;
        while (i < arr.length) {
            if ((arr[i].equals(value))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String units_string(String[] arr) {
        String s = "";
        int i_1 = 0;
        while (i_1 < arr.length) {
            if (i_1 > 0) {
                s = s + ", ";
            }
            s = s + arr[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static double round3(double x) {
        double y = x * 1000.0 + 0.5;
        int z = ((Number)(y)).intValue();
        double zf = ((Number)(z)).doubleValue();
        return zf / 1000.0;
    }

    static double convert_speed(double speed, String unit_from, String unit_to) {
        int from_index = index_of(((String[])(units)), unit_from);
        int to_index = index_of(((String[])(units)), unit_to);
        if (from_index < 0 || to_index < 0) {
            String msg = "Incorrect 'from_type' or 'to_type' value: " + unit_from + ", " + unit_to + "\nValid values are: " + String.valueOf(units_string(((String[])(units))));
            throw new RuntimeException(String.valueOf(msg));
        }
        double result = speed * speed_chart[from_index] * speed_chart_inverse[to_index];
        double r = round3(result);
        return r;
    }
    public static void main(String[] args) {
        units = ((String[])(new String[]{"km/h", "m/s", "mph", "knot"}));
        speed_chart = ((double[])(new double[]{1.0, 3.6, 1.609344, 1.852}));
        speed_chart_inverse = ((double[])(new double[]{1.0, 0.277777778, 0.621371192, 0.539956803}));
        System.out.println(_p(convert_speed(100.0, "km/h", "m/s")));
        System.out.println(_p(convert_speed(100.0, "km/h", "mph")));
        System.out.println(_p(convert_speed(100.0, "km/h", "knot")));
        System.out.println(_p(convert_speed(100.0, "m/s", "km/h")));
        System.out.println(_p(convert_speed(100.0, "m/s", "mph")));
        System.out.println(_p(convert_speed(100.0, "m/s", "knot")));
        System.out.println(_p(convert_speed(100.0, "mph", "km/h")));
        System.out.println(_p(convert_speed(100.0, "mph", "m/s")));
        System.out.println(_p(convert_speed(100.0, "mph", "knot")));
        System.out.println(_p(convert_speed(100.0, "knot", "km/h")));
        System.out.println(_p(convert_speed(100.0, "knot", "m/s")));
        System.out.println(_p(convert_speed(100.0, "knot", "mph")));
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
