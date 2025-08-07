public class Main {
    static double mass;
    static double acceleration;
    static double force;

    static double newtons_second_law_of_motion(double mass, double acceleration) {
        if (mass < 0.0 || acceleration < 0.0) {
            return 0.0;
        }
        return mass * acceleration;
    }
    public static void main(String[] args) {
        mass = 12.5;
        acceleration = 10.0;
        force = newtons_second_law_of_motion(mass, acceleration);
        System.out.println("The force is " + _p(force) + " N");
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
