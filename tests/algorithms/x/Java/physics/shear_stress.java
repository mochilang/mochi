public class Main {
    static class Result {
        String name;
        double value;
        Result(String name, double value) {
            this.name = name;
            this.value = value;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s}", String.valueOf(name), String.valueOf(value));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;

    static Result shear_stress(double stress, double tangential_force, double area) {
        int zeros = 0;
        if (stress == 0.0) {
            zeros = zeros + 1;
        }
        if (tangential_force == 0.0) {
            zeros = zeros + 1;
        }
        if (area == 0.0) {
            zeros = zeros + 1;
        }
        if (zeros != 1) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        } else         if (stress < 0.0) {
            throw new RuntimeException(String.valueOf("Stress cannot be negative"));
        } else         if (tangential_force < 0.0) {
            throw new RuntimeException(String.valueOf("Tangential Force cannot be negative"));
        } else         if (area < 0.0) {
            throw new RuntimeException(String.valueOf("Area cannot be negative"));
        } else         if (stress == 0.0) {
            return new Result("stress", tangential_force / area);
        } else         if (tangential_force == 0.0) {
            return new Result("tangential_force", stress * area);
        } else {
            return new Result("area", tangential_force / stress);
        }
    }

    static String str_result(Result r) {
        return "Result(name='" + r.name + "', value=" + _p(r.value) + ")";
    }
    public static void main(String[] args) {
        r1 = shear_stress(25.0, 100.0, 0.0);
        System.out.println(str_result(r1));
        r2 = shear_stress(0.0, 1600.0, 200.0);
        System.out.println(str_result(r2));
        r3 = shear_stress(1000.0, 0.0, 1200.0);
        System.out.println(str_result(r3));
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
