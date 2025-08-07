public class Main {
    static double GRAVITATIONAL_CONSTANT;
    static class Result {
        String kind;
        double value;
        Result(String kind, double value) {
            this.kind = kind;
            this.value = value;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'kind': '%s', 'value': %s}", String.valueOf(kind), String.valueOf(value));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;
    static Result r4;

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static Result gravitational_law(double force, double mass_1, double mass_2, double distance) {
        int zero_count = 0;
        if (force == 0.0) {
            zero_count = zero_count + 1;
        }
        if (mass_1 == 0.0) {
            zero_count = zero_count + 1;
        }
        if (mass_2 == 0.0) {
            zero_count = zero_count + 1;
        }
        if (distance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if (force < 0.0) {
            throw new RuntimeException(String.valueOf("Gravitational force can not be negative"));
        }
        if (distance < 0.0) {
            throw new RuntimeException(String.valueOf("Distance can not be negative"));
        }
        if (mass_1 < 0.0) {
            throw new RuntimeException(String.valueOf("Mass can not be negative"));
        }
        if (mass_2 < 0.0) {
            throw new RuntimeException(String.valueOf("Mass can not be negative"));
        }
        double product_of_mass = mass_1 * mass_2;
        if (force == 0.0) {
            double f = GRAVITATIONAL_CONSTANT * product_of_mass / (distance * distance);
            return new Result("force", f);
        }
        if (mass_1 == 0.0) {
            double m1 = force * (distance * distance) / (GRAVITATIONAL_CONSTANT * mass_2);
            return new Result("mass_1", m1);
        }
        if (mass_2 == 0.0) {
            double m2 = force * (distance * distance) / (GRAVITATIONAL_CONSTANT * mass_1);
            return new Result("mass_2", m2);
        }
        double d = sqrtApprox(GRAVITATIONAL_CONSTANT * product_of_mass / force);
        return new Result("distance", d);
    }
    public static void main(String[] args) {
        GRAVITATIONAL_CONSTANT = 6.6743e-11;
        r1 = gravitational_law(0.0, 5.0, 10.0, 20.0);
        r2 = gravitational_law(7367.382, 0.0, 74.0, 3048.0);
        r3 = gravitational_law(100.0, 5.0, 0.0, 3.0);
        r4 = gravitational_law(100.0, 5.0, 10.0, 0.0);
        System.out.println(r1.kind + " " + _p(r1.value));
        System.out.println(r2.kind + " " + _p(r2.value));
        System.out.println(r3.kind + " " + _p(r3.value));
        System.out.println(r4.kind + " " + _p(r4.value));
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
