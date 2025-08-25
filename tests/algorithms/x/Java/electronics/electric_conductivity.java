public class Main {
    static double ELECTRON_CHARGE = (double)(1.6021e-19);
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

    static Result electric_conductivity(double conductivity, double electron_conc, double mobility) {
        java.math.BigInteger zero_count = java.math.BigInteger.valueOf(0);
        if ((double)(conductivity) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(electron_conc) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(mobility) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if (zero_count.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        }
        if ((double)(conductivity) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Conductivity cannot be negative"));
        }
        if ((double)(electron_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Electron concentration cannot be negative"));
        }
        if ((double)(mobility) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("mobility cannot be negative"));
        }
        if ((double)(conductivity) == (double)(0.0)) {
            return new Result("conductivity", (double)((double)((double)(mobility) * (double)(electron_conc)) * (double)(ELECTRON_CHARGE)));
        }
        if ((double)(electron_conc) == (double)(0.0)) {
            return new Result("electron_conc", (double)((double)(conductivity) / (double)(((double)(mobility) * (double)(ELECTRON_CHARGE)))));
        }
        return new Result("mobility", (double)((double)(conductivity) / (double)(((double)(electron_conc) * (double)(ELECTRON_CHARGE)))));
    }
    public static void main(String[] args) {
        r1 = electric_conductivity((double)(25.0), (double)(100.0), (double)(0.0));
        r2 = electric_conductivity((double)(0.0), (double)(1600.0), (double)(200.0));
        r3 = electric_conductivity((double)(1000.0), (double)(0.0), (double)(1200.0));
        System.out.println(r1.kind + " " + _p(r1.value));
        System.out.println(r2.kind + " " + _p(r2.value));
        System.out.println(r3.kind + " " + _p(r3.value));
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
