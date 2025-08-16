public class Main {
    static double PI = (double)(3.141592653589793);

    static double arc_length(double angle, double radius) {
        return (double)((double)((double)(2.0) * (double)(PI)) * (double)(radius)) * (double)(((double)(angle) / (double)(360.0)));
    }
    public static void main(String[] args) {
        System.out.println(_p(arc_length((double)(45.0), (double)(5.0))));
        System.out.println(_p(arc_length((double)(120.0), (double)(15.0))));
        System.out.println(_p(arc_length((double)(90.0), (double)(10.0))));
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
