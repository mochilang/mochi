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

    static java.math.BigInteger trapped_rainwater(java.math.BigInteger[] heights) {
        if (new java.math.BigInteger(String.valueOf(heights.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(heights.length))) < 0) {
            if (heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                throw new RuntimeException(String.valueOf("No height can be negative"));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger length_1 = new java.math.BigInteger(String.valueOf(heights.length));
        java.math.BigInteger[] left_max_1 = ((java.math.BigInteger[])(make_list(new java.math.BigInteger(String.valueOf(length_1)), java.math.BigInteger.valueOf(0))));
left_max_1[(int)(0L)] = new java.math.BigInteger(String.valueOf(heights[_idx((heights).length, 0L)]));
        i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(length_1) < 0) {
            if (heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())].compareTo(left_max_1[_idx((left_max_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) > 0) {
left_max_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())]));
            } else {
left_max_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(left_max_1[_idx((left_max_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] right_max_1 = ((java.math.BigInteger[])(make_list(new java.math.BigInteger(String.valueOf(length_1)), java.math.BigInteger.valueOf(0))));
        java.math.BigInteger last_1 = new java.math.BigInteger(String.valueOf(length_1.subtract(java.math.BigInteger.valueOf(1))));
right_max_1[(int)(((java.math.BigInteger)(last_1)).longValue())] = new java.math.BigInteger(String.valueOf(heights[_idx((heights).length, ((java.math.BigInteger)(last_1)).longValue())]));
        i_3 = new java.math.BigInteger(String.valueOf(last_1.subtract(java.math.BigInteger.valueOf(1))));
        while (i_3.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            if (heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())].compareTo(right_max_1[_idx((right_max_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())]) > 0) {
right_max_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())]));
            } else {
right_max_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(right_max_1[_idx((right_max_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())]));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.subtract(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger total_1 = java.math.BigInteger.valueOf(0);
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(length_1) < 0) {
            java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(left_max_1[_idx((left_max_1).length, ((java.math.BigInteger)(i_3)).longValue())]));
            java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(right_max_1[_idx((right_max_1).length, ((java.math.BigInteger)(i_3)).longValue())]));
            java.math.BigInteger smaller_1 = new java.math.BigInteger(String.valueOf(left_1.compareTo(right_1) < 0 ? left_1 : right_1));
            total_1 = new java.math.BigInteger(String.valueOf(total_1.add((smaller_1.subtract(heights[_idx((heights).length, ((java.math.BigInteger)(i_3)).longValue())])))));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(total_1));
    }
    public static void main(String[] args) {
        System.out.println(_p(trapped_rainwater(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})))));
        System.out.println(_p(trapped_rainwater(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(4)})))));
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
