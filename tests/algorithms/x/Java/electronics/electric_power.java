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
    static Result r4;
    static Result r5;

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(-x);
        }
        return (double)(x);
    }

    static double pow10(java.math.BigInteger n) {
        double p = (double)(1.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            p = (double)((double)(p) * (double)(10.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(p);
    }

    static double round_to(double x, java.math.BigInteger n) {
        double m = (double)(pow10(new java.math.BigInteger(String.valueOf(n))));
        return (double)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m));
    }

    static Result electric_power(double voltage, double current, double power) {
        java.math.BigInteger zeros = java.math.BigInteger.valueOf(0);
        if ((double)(voltage) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(current) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(power) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if (zeros.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("Exactly one argument must be 0"));
        } else         if ((double)(power) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Power cannot be negative in any electrical/electronics system"));
        } else         if ((double)(voltage) == (double)(0.0)) {
            return new Result("voltage", (double)((double)(power) / (double)(current)));
        } else         if ((double)(current) == (double)(0.0)) {
            return new Result("current", (double)((double)(power) / (double)(voltage)));
        } else         if ((double)(power) == (double)(0.0)) {
            double p_2 = (double)(absf((double)((double)(voltage) * (double)(current))));
            return new Result("power", (double)(round_to((double)(p_2), java.math.BigInteger.valueOf(2))));
        } else {
            throw new RuntimeException(String.valueOf("Unhandled case"));
        }
    }

    static String str_result(Result r) {
        return "Result(name='" + r.name + "', value=" + _p(r.value) + ")";
    }
    public static void main(String[] args) {
        r1 = electric_power((double)(0.0), (double)(2.0), (double)(5.0));
        System.out.println(str_result(r1));
        r2 = electric_power((double)(2.0), (double)(2.0), (double)(0.0));
        System.out.println(str_result(r2));
        r3 = electric_power((double)(-2.0), (double)(3.0), (double)(0.0));
        System.out.println(str_result(r3));
        r4 = electric_power((double)(2.2), (double)(2.2), (double)(0.0));
        System.out.println(str_result(r4));
        r5 = electric_power((double)(2.0), (double)(0.0), (double)(6.0));
        System.out.println(str_result(r5));
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
