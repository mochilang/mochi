public class Main {
    static class LcsResult {
        java.math.BigInteger length;
        String sequence;
        LcsResult(java.math.BigInteger length, String sequence) {
            this.length = length;
            this.sequence = sequence;
        }
        LcsResult() {}
        @Override public String toString() {
            return String.format("{'length': %s, 'sequence': '%s'}", String.valueOf(length), String.valueOf(sequence));
        }
    }

    static String a = "AGGTAB";
    static String b = "GXTXAYB";
    static LcsResult res;

    static java.math.BigInteger[][] zeros_matrix(java.math.BigInteger rows, java.math.BigInteger cols) {
        java.math.BigInteger[][] matrix = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(rows) <= 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(cols) <= 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            matrix = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[][])(matrix));
    }

    static LcsResult longest_common_subsequence(String x, String y) {
        java.math.BigInteger m = new java.math.BigInteger(String.valueOf(_runeLen(x)));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(y)));
        java.math.BigInteger[][] dp_1 = ((java.math.BigInteger[][])(zeros_matrix(new java.math.BigInteger(String.valueOf(m)), new java.math.BigInteger(String.valueOf(n_1)))));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(m) <= 0) {
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(n_1) <= 0) {
                if ((x.substring((int)(((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())+1).equals(y.substring((int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())+1)))) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(java.math.BigInteger.valueOf(1))));
                } else                 if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())].compareTo(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) > 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]));
                } else {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        String seq_1 = "";
        java.math.BigInteger i2_1 = new java.math.BigInteger(String.valueOf(m));
        java.math.BigInteger j2_1 = new java.math.BigInteger(String.valueOf(n_1));
        while (i2_1.compareTo(java.math.BigInteger.valueOf(0)) > 0 && j2_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            if ((x.substring((int)(((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())+1).equals(y.substring((int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())+1)))) {
                seq_1 = x.substring((int)(((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())+1) + seq_1;
                i2_1 = new java.math.BigInteger(String.valueOf(i2_1.subtract(java.math.BigInteger.valueOf(1))));
                j2_1 = new java.math.BigInteger(String.valueOf(j2_1.subtract(java.math.BigInteger.valueOf(1))));
            } else             if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())].compareTo(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i2_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i2_1)).longValue())]).length, ((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) >= 0) {
                i2_1 = new java.math.BigInteger(String.valueOf(i2_1.subtract(java.math.BigInteger.valueOf(1))));
            } else {
                j2_1 = new java.math.BigInteger(String.valueOf(j2_1.subtract(java.math.BigInteger.valueOf(1))));
            }
        }
        return new LcsResult(new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(m)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(m)).longValue())]).length, ((java.math.BigInteger)(n_1)).longValue())])), seq_1);
    }
    public static void main(String[] args) {
        res = longest_common_subsequence(a, b);
        System.out.println("len = " + _p(res.length) + ", sub-sequence = " + res.sequence);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
