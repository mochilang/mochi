public class Main {
    static double COULOMBS_CONSTANT = (double)(8988000000.0);

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(-x);
        }
        return (double)(x);
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)(x);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(guess_1);
    }

    static java.util.Map<String,Double> coulombs_law(double force, double charge1, double charge2, double distance) {
        double charge_product = ((Number)(Math.abs((double)(charge1) * (double)(charge2)))).doubleValue();
        java.math.BigInteger zero_count_1 = java.math.BigInteger.valueOf(0);
        if ((double)(force) == (double)(0.0)) {
            zero_count_1 = new java.math.BigInteger(String.valueOf(zero_count_1.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(charge1) == (double)(0.0)) {
            zero_count_1 = new java.math.BigInteger(String.valueOf(zero_count_1.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(charge2) == (double)(0.0)) {
            zero_count_1 = new java.math.BigInteger(String.valueOf(zero_count_1.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(distance) == (double)(0.0)) {
            zero_count_1 = new java.math.BigInteger(String.valueOf(zero_count_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (zero_count_1.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Distance cannot be negative"));
        }
        if ((double)(force) == (double)(0.0)) {
            double f_1 = (double)((double)((double)(COULOMBS_CONSTANT) * (double)(charge_product)) / (double)(((double)(distance) * (double)(distance))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("force", (double)(f_1)); }}));
        }
        if ((double)(charge1) == (double)(0.0)) {
            double c1_1 = (double)((double)(Math.abs(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(COULOMBS_CONSTANT) * (double)(charge2))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("charge1", (double)(c1_1)); }}));
        }
        if ((double)(charge2) == (double)(0.0)) {
            double c2_1 = (double)((double)(Math.abs(force) * (double)(((double)(distance) * (double)(distance)))) / (double)(((double)(COULOMBS_CONSTANT) * (double)(charge1))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("charge2", (double)(c2_1)); }}));
        }
        double d_1 = (double)(sqrtApprox((double)((double)((double)(COULOMBS_CONSTANT) * (double)(charge_product)) / Math.abs(force))));
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("distance", (double)(d_1)); }}));
    }

    static void print_map(java.util.Map<String,Double> m) {
        for (String k : m.keySet()) {
            System.out.println("{\"" + k + "\": " + _p(((double)(m).getOrDefault(k, 0.0))) + "}");
        }
    }
    public static void main(String[] args) {
        print_map(coulombs_law((double)(0.0), (double)(3.0), (double)(5.0), (double)(2000.0)));
        print_map(coulombs_law((double)(10.0), (double)(3.0), (double)(5.0), (double)(0.0)));
        print_map(coulombs_law((double)(10.0), (double)(0.0), (double)(5.0), (double)(2000.0)));
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
