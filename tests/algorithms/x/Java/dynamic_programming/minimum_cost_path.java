public class Main {
    static java.math.BigInteger[][] m1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(2)}))}));
    static java.math.BigInteger[][] m2 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)}))}));

    static java.math.BigInteger min_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) < 0) {
            return new java.math.BigInteger(String.valueOf(a));
        }
        return new java.math.BigInteger(String.valueOf(b));
    }

    static java.math.BigInteger minimum_cost_path(java.math.BigInteger[][] matrix) {
        java.math.BigInteger rows = new java.math.BigInteger(String.valueOf(matrix.length));
        java.math.BigInteger cols_1 = new java.math.BigInteger(String.valueOf(matrix[_idx((matrix).length, 0L)].length));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(cols_1) < 0) {
            java.math.BigInteger[] row0_1 = ((java.math.BigInteger[])(matrix[_idx((matrix).length, 0L)]));
row0_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(row0_1[_idx((row0_1).length, ((java.math.BigInteger)(j_1)).longValue())].add(row0_1[_idx((row0_1).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())])));
matrix[(int)(0L)] = ((java.math.BigInteger[])(row0_1));
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(rows) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1)).longValue())]));
row_1[(int)(0L)] = new java.math.BigInteger(String.valueOf(row_1[_idx((row_1).length, 0L)].add(matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, 0L)])));
matrix[(int)(((java.math.BigInteger)(i_1)).longValue())] = ((java.math.BigInteger[])(row_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(rows) < 0) {
            java.math.BigInteger[] row_3 = ((java.math.BigInteger[])(matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1)).longValue())]));
            j_1 = java.math.BigInteger.valueOf(1);
            while (j_1.compareTo(cols_1) < 0) {
                java.math.BigInteger up_1 = new java.math.BigInteger(String.valueOf(matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((matrix[_idx((matrix).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())]));
                java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(row_3[_idx((row_3).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
                java.math.BigInteger best_1 = new java.math.BigInteger(String.valueOf(min_int(new java.math.BigInteger(String.valueOf(up_1)), new java.math.BigInteger(String.valueOf(left_1)))));
row_3[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(row_3[_idx((row_3).length, ((java.math.BigInteger)(j_1)).longValue())].add(best_1)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
matrix[(int)(((java.math.BigInteger)(i_1)).longValue())] = ((java.math.BigInteger[])(row_3));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(matrix[_idx((matrix).length, ((java.math.BigInteger)(rows.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((matrix[_idx((matrix).length, ((java.math.BigInteger)(rows.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(cols_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(_p(minimum_cost_path(((java.math.BigInteger[][])(m1)))));
        System.out.println(_p(minimum_cost_path(((java.math.BigInteger[][])(m2)))));
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
