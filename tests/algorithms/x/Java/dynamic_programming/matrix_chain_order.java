public class Main {
    static class MatrixChainResult {
        java.math.BigInteger[][] matrix;
        java.math.BigInteger[][] solution;
        MatrixChainResult(java.math.BigInteger[][] matrix, java.math.BigInteger[][] solution) {
            this.matrix = matrix;
            this.solution = solution;
        }
        MatrixChainResult() {}
        @Override public String toString() {
            return String.format("{'matrix': %s, 'solution': %s}", String.valueOf(matrix), String.valueOf(solution));
        }
    }


    static java.math.BigInteger[][] make_2d(java.math.BigInteger n) {
        java.math.BigInteger[][] res = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            res = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[][])(res));
    }

    static MatrixChainResult matrix_chain_order(java.math.BigInteger[] arr) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger[][] m_1 = ((java.math.BigInteger[][])(make_2d(new java.math.BigInteger(String.valueOf(n)))));
        java.math.BigInteger[][] s_1 = ((java.math.BigInteger[][])(make_2d(new java.math.BigInteger(String.valueOf(n)))));
        java.math.BigInteger chain_length_1 = java.math.BigInteger.valueOf(2);
        while (chain_length_1.compareTo(n) < 0) {
            java.math.BigInteger a_1 = java.math.BigInteger.valueOf(1);
            while (a_1.compareTo(n.subtract(chain_length_1).add(java.math.BigInteger.valueOf(1))) < 0) {
                java.math.BigInteger b_1 = new java.math.BigInteger(String.valueOf(a_1.add(chain_length_1).subtract(java.math.BigInteger.valueOf(1))));
m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())][(int)(((java.math.BigInteger)(b_1)).longValue())] = java.math.BigInteger.valueOf(1000000000);
                java.math.BigInteger c_1 = new java.math.BigInteger(String.valueOf(a_1));
                while (c_1.compareTo(b_1) < 0) {
                    java.math.BigInteger cost_1 = new java.math.BigInteger(String.valueOf(m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())][_idx((m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())]).length, ((java.math.BigInteger)(c_1)).longValue())].add(m_1[_idx((m_1).length, ((java.math.BigInteger)(c_1.add(java.math.BigInteger.valueOf(1)))).longValue())][_idx((m_1[_idx((m_1).length, ((java.math.BigInteger)(c_1.add(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(b_1)).longValue())]).add(arr[_idx((arr).length, ((java.math.BigInteger)(a_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].multiply(arr[_idx((arr).length, ((java.math.BigInteger)(c_1)).longValue())]).multiply(arr[_idx((arr).length, ((java.math.BigInteger)(b_1)).longValue())]))));
                    if (cost_1.compareTo(m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())][_idx((m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())]).length, ((java.math.BigInteger)(b_1)).longValue())]) < 0) {
m_1[_idx((m_1).length, ((java.math.BigInteger)(a_1)).longValue())][(int)(((java.math.BigInteger)(b_1)).longValue())] = new java.math.BigInteger(String.valueOf(cost_1));
s_1[_idx((s_1).length, ((java.math.BigInteger)(a_1)).longValue())][(int)(((java.math.BigInteger)(b_1)).longValue())] = new java.math.BigInteger(String.valueOf(c_1));
                    }
                    c_1 = new java.math.BigInteger(String.valueOf(c_1.add(java.math.BigInteger.valueOf(1))));
                }
                a_1 = new java.math.BigInteger(String.valueOf(a_1.add(java.math.BigInteger.valueOf(1))));
            }
            chain_length_1 = new java.math.BigInteger(String.valueOf(chain_length_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new MatrixChainResult(((java.math.BigInteger[][])(m_1)), ((java.math.BigInteger[][])(s_1)));
    }

    static String optimal_parenthesization(java.math.BigInteger[][] s, java.math.BigInteger i, java.math.BigInteger j) {
        if (i.compareTo(j) == 0) {
            return "A" + _p(i);
        } else {
            String left = String.valueOf(optimal_parenthesization(((java.math.BigInteger[][])(s)), new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(s[_idx((s).length, ((java.math.BigInteger)(i)).longValue())][_idx((s[_idx((s).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())]))));
            String right = String.valueOf(optimal_parenthesization(((java.math.BigInteger[][])(s)), new java.math.BigInteger(String.valueOf(s[_idx((s).length, ((java.math.BigInteger)(i)).longValue())][_idx((s[_idx((s).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())].add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(j))));
            return "( " + left + " " + right + " )";
        }
    }

    static void main() {
        java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(35), java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(25)}));
        java.math.BigInteger n_2 = new java.math.BigInteger(String.valueOf(arr.length));
        MatrixChainResult res_2 = matrix_chain_order(((java.math.BigInteger[])(arr)));
        java.math.BigInteger[][] m_3 = ((java.math.BigInteger[][])(res_2.matrix));
        java.math.BigInteger[][] s_3 = ((java.math.BigInteger[][])(res_2.solution));
        System.out.println("No. of Operation required: " + _p(_geto(m_3[_idx((m_3).length, 1L)], ((Number)(n_2.subtract(java.math.BigInteger.valueOf(1)))).intValue())));
        String seq_1 = String.valueOf(optimal_parenthesization(((java.math.BigInteger[][])(s_3)), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n_2.subtract(java.math.BigInteger.valueOf(1))))));
        System.out.println(seq_1);
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
