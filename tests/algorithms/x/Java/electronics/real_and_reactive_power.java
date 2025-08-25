public class Main {

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)(x);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(guess_1);
    }

    static double real_power(double apparent_power, double power_factor) {
        if ((double)(power_factor) < (double)(-1.0) || (double)(power_factor) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("power_factor must be a valid float value between -1 and 1."));
        }
        return (double)((double)(apparent_power) * (double)(power_factor));
    }

    static double reactive_power(double apparent_power, double power_factor) {
        if ((double)(power_factor) < (double)(-1.0) || (double)(power_factor) > (double)(1.0)) {
            throw new RuntimeException(String.valueOf("power_factor must be a valid float value between -1 and 1."));
        }
        return (double)((double)(apparent_power) * (double)(sqrt((double)((double)(1.0) - (double)((double)(power_factor) * (double)(power_factor))))));
    }
    public static void main(String[] args) {
        System.out.println(_p(real_power((double)(100.0), (double)(0.9))));
        System.out.println(_p(real_power((double)(0.0), (double)(0.8))));
        System.out.println(_p(real_power((double)(100.0), (double)(-0.9))));
        System.out.println(_p(reactive_power((double)(100.0), (double)(0.9))));
        System.out.println(_p(reactive_power((double)(0.0), (double)(0.8))));
        System.out.println(_p(reactive_power((double)(100.0), (double)(-0.9))));
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
