public class Main {

    static double resistor_parallel(double[] resistors) {
        double sum = (double)(0.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(resistors.length))) < 0) {
            double r_1 = (double)(resistors[_idx((resistors).length, ((java.math.BigInteger)(i_1)).longValue())]);
            if ((double)(r_1) <= (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Resistor at index " + _p(i_1) + " has a negative or zero value!"));
            }
            sum = (double)((double)(sum) + (double)((double)(1.0) / (double)(r_1)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)((double)(1.0) / (double)(sum));
    }

    static double resistor_series(double[] resistors) {
        double sum_1 = (double)(0.0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(resistors.length))) < 0) {
            double r_3 = (double)(resistors[_idx((resistors).length, ((java.math.BigInteger)(i_3)).longValue())]);
            if ((double)(r_3) < (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Resistor at index " + _p(i_3) + " has a negative value!"));
            }
            sum_1 = (double)((double)(sum_1) + (double)(r_3));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum_1);
    }

    static void main() {
        double[] resistors = ((double[])(new double[]{(double)(3.21389), (double)(2.0), (double)(3.0)}));
        System.out.println("Parallel: " + _p(resistor_parallel(((double[])(resistors)))));
        System.out.println("Series: " + _p(resistor_series(((double[])(resistors)))));
    }
    public static void main(String[] args) {
        main();
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
