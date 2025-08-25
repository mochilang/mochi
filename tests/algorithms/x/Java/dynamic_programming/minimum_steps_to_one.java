public class Main {

    static java.math.BigInteger[] make_list(java.math.BigInteger len, java.math.BigInteger value) {
        java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(len) < 0) {
            arr = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(value)))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(arr));
    }

    static java.math.BigInteger min_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) < 0) {
            return new java.math.BigInteger(String.valueOf(a));
        }
        return new java.math.BigInteger(String.valueOf(b));
    }

    static java.math.BigInteger min_steps_to_one(java.math.BigInteger number) {
        if (number.compareTo(java.math.BigInteger.valueOf(0)) <= 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger[] table_1 = ((java.math.BigInteger[])(make_list(new java.math.BigInteger(String.valueOf(number.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(number.add(java.math.BigInteger.valueOf(1)))))));
table_1[(int)(1L)] = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(number) < 0) {
table_1[(int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())] = new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())])), new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3)).longValue())].add(java.math.BigInteger.valueOf(1)))))));
            if (i_3.multiply(java.math.BigInteger.valueOf(2)).compareTo(number) <= 0) {
table_1[(int)(((java.math.BigInteger)(i_3.multiply(java.math.BigInteger.valueOf(2)))).longValue())] = new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3.multiply(java.math.BigInteger.valueOf(2)))).longValue())])), new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3)).longValue())].add(java.math.BigInteger.valueOf(1)))))));
            }
            if (i_3.multiply(java.math.BigInteger.valueOf(3)).compareTo(number) <= 0) {
table_1[(int)(((java.math.BigInteger)(i_3.multiply(java.math.BigInteger.valueOf(3)))).longValue())] = new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3.multiply(java.math.BigInteger.valueOf(3)))).longValue())])), new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(i_3)).longValue())].add(java.math.BigInteger.valueOf(1)))))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(number)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(_p(min_steps_to_one(java.math.BigInteger.valueOf(10))));
        System.out.println(_p(min_steps_to_one(java.math.BigInteger.valueOf(15))));
        System.out.println(_p(min_steps_to_one(java.math.BigInteger.valueOf(6))));
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
