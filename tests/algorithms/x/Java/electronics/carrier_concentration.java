public class Main {
    static class CarrierResult {
        String name;
        double value;
        CarrierResult(String name, double value) {
            this.name = name;
            this.value = value;
        }
        CarrierResult() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s}", String.valueOf(name), String.valueOf(value));
        }
    }

    static CarrierResult r1;
    static CarrierResult r2;
    static CarrierResult r3;

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(guess);
    }

    static CarrierResult carrier_concentration(double electron_conc, double hole_conc, double intrinsic_conc) {
        java.math.BigInteger zero_count = java.math.BigInteger.valueOf(0);
        if ((double)(electron_conc) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(hole_conc) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(intrinsic_conc) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if (zero_count.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        }
        if ((double)(electron_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Electron concentration cannot be negative in a semiconductor"));
        }
        if ((double)(hole_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Hole concentration cannot be negative in a semiconductor"));
        }
        if ((double)(intrinsic_conc) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Intrinsic concentration cannot be negative in a semiconductor"));
        }
        if ((double)(electron_conc) == (double)(0.0)) {
            return new CarrierResult("electron_conc", (double)((double)(((double)(intrinsic_conc) * (double)(intrinsic_conc))) / (double)(hole_conc)));
        }
        if ((double)(hole_conc) == (double)(0.0)) {
            return new CarrierResult("hole_conc", (double)((double)(((double)(intrinsic_conc) * (double)(intrinsic_conc))) / (double)(electron_conc)));
        }
        if ((double)(intrinsic_conc) == (double)(0.0)) {
            return new CarrierResult("intrinsic_conc", (double)(sqrtApprox((double)((double)(electron_conc) * (double)(hole_conc)))));
        }
        return new CarrierResult("", (double)(-1.0));
    }
    public static void main(String[] args) {
        r1 = carrier_concentration((double)(25.0), (double)(100.0), (double)(0.0));
        System.out.println(r1.name + ", " + _p(r1.value));
        r2 = carrier_concentration((double)(0.0), (double)(1600.0), (double)(200.0));
        System.out.println(r2.name + ", " + _p(r2.value));
        r3 = carrier_concentration((double)(1000.0), (double)(0.0), (double)(1200.0));
        System.out.println(r3.name + ", " + _p(r3.value));
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
