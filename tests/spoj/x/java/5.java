public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String next_pal(String s) {
        java.util.Map<String,java.math.BigInteger> digitMap = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>() {{ put("0", java.math.BigInteger.valueOf(0)); put("1", java.math.BigInteger.valueOf(1)); put("2", java.math.BigInteger.valueOf(2)); put("3", java.math.BigInteger.valueOf(3)); put("4", java.math.BigInteger.valueOf(4)); put("5", java.math.BigInteger.valueOf(5)); put("6", java.math.BigInteger.valueOf(6)); put("7", java.math.BigInteger.valueOf(7)); put("8", java.math.BigInteger.valueOf(8)); put("9", java.math.BigInteger.valueOf(9)); }}));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        java.math.BigInteger[] num_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        for (java.math.BigInteger i = java.math.BigInteger.valueOf(0); i.compareTo(n_1) < 0; i = i.add(java.math.BigInteger.ONE)) {
            num_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(num_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(((Number)(((java.math.BigInteger)(digitMap).get(_substr(s, (int)(((java.math.BigInteger)(i)).longValue()), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue())))).toArray(java.math.BigInteger[]::new)));
        }
        boolean all9_1 = true;
        for (java.math.BigInteger d : num_1) {
            if (d.compareTo(java.math.BigInteger.valueOf(9)) != 0) {
                all9_1 = false;
                break;
            }
        }
        if (all9_1) {
            String res_1 = "1";
            for (java.math.BigInteger _v = java.math.BigInteger.valueOf(0); _v.compareTo((n_1.subtract(java.math.BigInteger.valueOf(1)))) < 0; _v = _v.add(java.math.BigInteger.ONE)) {
                res_1 = res_1 + "0";
            }
            res_1 = res_1 + "1";
            return res_1;
        }
        java.math.BigInteger left_1 = n_1.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1));
        java.math.BigInteger right_1 = n_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(0)) == 0 ? n_1.divide(java.math.BigInteger.valueOf(2)) : n_1.divide(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1));
        while (left_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 && right_1.compareTo(n_1) < 0 && num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())].compareTo(num_1[_idx((num_1).length, ((java.math.BigInteger)(right_1)).longValue())]) == 0) {
            left_1 = left_1.subtract(java.math.BigInteger.valueOf(1));
            right_1 = right_1.add(java.math.BigInteger.valueOf(1));
        }
        boolean smaller_1 = left_1.compareTo(java.math.BigInteger.valueOf(0)) < 0 || num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())].compareTo(num_1[_idx((num_1).length, ((java.math.BigInteger)(right_1)).longValue())]) < 0;
        left_1 = n_1.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1));
        right_1 = n_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(0)) == 0 ? n_1.divide(java.math.BigInteger.valueOf(2)) : n_1.divide(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1));
        while (left_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
num_1[(int)(((java.math.BigInteger)(right_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())];
            left_1 = left_1.subtract(java.math.BigInteger.valueOf(1));
            right_1 = right_1.add(java.math.BigInteger.valueOf(1));
        }
        if (smaller_1) {
            java.math.BigInteger carry_1 = java.math.BigInteger.valueOf(1);
            left_1 = n_1.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1));
            if (n_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                java.math.BigInteger mid_1 = n_1.divide(java.math.BigInteger.valueOf(2));
num_1[(int)(((java.math.BigInteger)(mid_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(mid_1)).longValue())].add(carry_1);
                carry_1 = num_1[_idx((num_1).length, ((java.math.BigInteger)(mid_1)).longValue())].divide(java.math.BigInteger.valueOf(10));
num_1[(int)(((java.math.BigInteger)(mid_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(mid_1)).longValue())].remainder(java.math.BigInteger.valueOf(10));
                right_1 = mid_1.add(java.math.BigInteger.valueOf(1));
            } else {
                right_1 = n_1.divide(java.math.BigInteger.valueOf(2));
            }
            while (left_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
num_1[(int)(((java.math.BigInteger)(left_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())].add(carry_1);
                carry_1 = num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())].divide(java.math.BigInteger.valueOf(10));
num_1[(int)(((java.math.BigInteger)(left_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())].remainder(java.math.BigInteger.valueOf(10));
num_1[(int)(((java.math.BigInteger)(right_1)).longValue())] = num_1[_idx((num_1).length, ((java.math.BigInteger)(left_1)).longValue())];
                left_1 = left_1.subtract(java.math.BigInteger.valueOf(1));
                right_1 = right_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        String out_1 = "";
        for (java.math.BigInteger d : num_1) {
            out_1 = out_1 + _p(d);
        }
        return out_1;
    }

    static java.math.BigInteger parseIntStr(String str) {
        java.util.Map<String,java.math.BigInteger> digits = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>() {{ put("0", java.math.BigInteger.valueOf(0)); put("1", java.math.BigInteger.valueOf(1)); put("2", java.math.BigInteger.valueOf(2)); put("3", java.math.BigInteger.valueOf(3)); put("4", java.math.BigInteger.valueOf(4)); put("5", java.math.BigInteger.valueOf(5)); put("6", java.math.BigInteger.valueOf(6)); put("7", java.math.BigInteger.valueOf(7)); put("8", java.math.BigInteger.valueOf(8)); put("9", java.math.BigInteger.valueOf(9)); }}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger n_3 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(str)))) < 0) {
            n_3 = n_3.multiply(java.math.BigInteger.valueOf(10)).add(new java.math.BigInteger(String.valueOf((((Number)(((java.math.BigInteger)(digits).get(_substr(str, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()))));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return n_3;
    }

    static void main() {
        String tStr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((tStr.equals(""))) {
            return;
        }
        java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(tStr)));
        for (java.math.BigInteger _v = java.math.BigInteger.valueOf(0); _v.compareTo(t_1) < 0; _v = _v.add(java.math.BigInteger.ONE)) {
            String s_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            System.out.println(next_pal(s_1));
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
