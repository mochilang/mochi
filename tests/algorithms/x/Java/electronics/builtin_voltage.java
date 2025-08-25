public class Main {
    static double BOLTZMANN;
    static double ELECTRON_VOLT;
    static double TEMPERATURE = (double)(300.0);

    static double pow10(java.math.BigInteger n) {
        double result = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            result = (double)((double)(result) * (double)(10.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(result);
    }

    static double ln_series(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_1 = (double)(t);
        double sum_1 = (double)(0.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(19)) <= 0) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(n_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t)) * (double)(t));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(2))));
        }
        return (double)((double)(2.0) * (double)(sum_1));
    }

    static double ln(double x) {
        double y = (double)(x);
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while ((double)(y) >= (double)(10.0)) {
            y = (double)((double)(y) / (double)(10.0));
            k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
        }
        while ((double)(y) < (double)(1.0)) {
            y = (double)((double)(y) * (double)(10.0));
            k_1 = new java.math.BigInteger(String.valueOf(k_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return (double)((double)(ln_series((double)(y))) + (double)((double)((((Number)(k_1)).doubleValue())) * (double)(ln_series((double)(10.0)))));
    }

    static double builtin_voltage(double donor_conc, double acceptor_conc, double intrinsic_conc) {
        if ((double)(donor_conc) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Donor concentration should be positive"));
        }
        if ((double)(acceptor_conc) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Acceptor concentration should be positive"));
        }
        if ((double)(intrinsic_conc) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Intrinsic concentration should be positive"));
        }
        if ((double)(donor_conc) <= (double)(intrinsic_conc)) {
            throw new RuntimeException(String.valueOf("Donor concentration should be greater than intrinsic concentration"));
        }
        if ((double)(acceptor_conc) <= (double)(intrinsic_conc)) {
            throw new RuntimeException(String.valueOf("Acceptor concentration should be greater than intrinsic concentration"));
        }
        return (double)((double)((double)((double)(BOLTZMANN) * (double)(TEMPERATURE)) * (double)(Math.log((double)(((double)(donor_conc) * (double)(acceptor_conc))) / (double)(((double)(intrinsic_conc) * (double)(intrinsic_conc)))))) / (double)(ELECTRON_VOLT));
    }
    public static void main(String[] args) {
        BOLTZMANN = (double)((double)(1.380649) / (double)(pow10(java.math.BigInteger.valueOf(23))));
        ELECTRON_VOLT = (double)((double)(1.602176634) / (double)(pow10(java.math.BigInteger.valueOf(19))));
        System.out.println(_p(builtin_voltage((double)(pow10(java.math.BigInteger.valueOf(17))), (double)(pow10(java.math.BigInteger.valueOf(17))), (double)(pow10(java.math.BigInteger.valueOf(10))))));
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
