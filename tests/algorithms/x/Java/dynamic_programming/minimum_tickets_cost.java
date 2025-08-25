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

    static java.math.BigInteger max_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) > 0) {
            return new java.math.BigInteger(String.valueOf(a));
        } else {
            return new java.math.BigInteger(String.valueOf(b));
        }
    }

    static java.math.BigInteger min_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) < 0) {
            return new java.math.BigInteger(String.valueOf(a));
        } else {
            return new java.math.BigInteger(String.valueOf(b));
        }
    }

    static java.math.BigInteger min3(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger c) {
        return new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b))))), new java.math.BigInteger(String.valueOf(c)))));
    }

    static java.math.BigInteger minimum_tickets_cost(java.math.BigInteger[] days, java.math.BigInteger[] costs) {
        if (new java.math.BigInteger(String.valueOf(days.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger last_day_1 = new java.math.BigInteger(String.valueOf(days[_idx((days).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(days.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
        java.math.BigInteger[] dp_1 = ((java.math.BigInteger[])(make_list(new java.math.BigInteger(String.valueOf(last_day_1.add(java.math.BigInteger.valueOf(1)))), java.math.BigInteger.valueOf(0))));
        java.math.BigInteger day_index_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger d_1 = java.math.BigInteger.valueOf(1);
        while (d_1.compareTo(last_day_1) <= 0) {
            if (day_index_1.compareTo(new java.math.BigInteger(String.valueOf(days.length))) < 0 && d_1.compareTo(days[_idx((days).length, ((java.math.BigInteger)(day_index_1)).longValue())]) == 0) {
                java.math.BigInteger cost1_1 = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(d_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(costs[_idx((costs).length, 0L)])));
                java.math.BigInteger cost7_1 = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(max_int(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(d_1.subtract(java.math.BigInteger.valueOf(7))))))).longValue())].add(costs[_idx((costs).length, 1L)])));
                java.math.BigInteger cost30_1 = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(max_int(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(d_1.subtract(java.math.BigInteger.valueOf(30))))))).longValue())].add(costs[_idx((costs).length, 2L)])));
dp_1[(int)(((java.math.BigInteger)(d_1)).longValue())] = new java.math.BigInteger(String.valueOf(min3(new java.math.BigInteger(String.valueOf(cost1_1)), new java.math.BigInteger(String.valueOf(cost7_1)), new java.math.BigInteger(String.valueOf(cost30_1)))));
                day_index_1 = new java.math.BigInteger(String.valueOf(day_index_1.add(java.math.BigInteger.valueOf(1))));
            } else {
dp_1[(int)(((java.math.BigInteger)(d_1)).longValue())] = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(d_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
            }
            d_1 = new java.math.BigInteger(String.valueOf(d_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(last_day_1)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(_p(minimum_tickets_cost(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(20)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(15)})))));
        System.out.println(_p(minimum_tickets_cost(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(31)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(15)})))));
        System.out.println(_p(minimum_tickets_cost(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(31)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(90), java.math.BigInteger.valueOf(150)})))));
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
