public class Main {

    static java.math.BigInteger min3(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger c) {
        java.math.BigInteger m = new java.math.BigInteger(String.valueOf(a));
        if (b.compareTo(m) < 0) {
            m = new java.math.BigInteger(String.valueOf(b));
        }
        if (c.compareTo(m) < 0) {
            m = new java.math.BigInteger(String.valueOf(c));
        }
        return new java.math.BigInteger(String.valueOf(m));
    }

    static java.math.BigInteger helper(String word1, String word2, java.math.BigInteger[][] cache, java.math.BigInteger i, java.math.BigInteger j, java.math.BigInteger len1, java.math.BigInteger len2) {
        if (i.compareTo(len1) >= 0) {
            return new java.math.BigInteger(String.valueOf(len2.subtract(j)));
        }
        if (j.compareTo(len2) >= 0) {
            return new java.math.BigInteger(String.valueOf(len1.subtract(i)));
        }
        if (cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())][_idx((cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())].compareTo(((java.math.BigInteger.valueOf(1)).negate())) != 0) {
            return new java.math.BigInteger(String.valueOf(cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())][_idx((cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())]));
        }
        java.math.BigInteger diff_1 = java.math.BigInteger.valueOf(0);
        if (!(_substr(word1, (int)(((java.math.BigInteger)(i)).longValue()), (int)(((java.math.BigInteger)(i.add(java.math.BigInteger.valueOf(1)))).longValue())).equals(_substr(word2, (int)(((java.math.BigInteger)(j)).longValue()), (int)(((java.math.BigInteger)(j.add(java.math.BigInteger.valueOf(1)))).longValue()))))) {
            diff_1 = java.math.BigInteger.valueOf(1);
        }
        java.math.BigInteger delete_cost_1 = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(helper(word1, word2, ((java.math.BigInteger[][])(cache)), new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(j)), new java.math.BigInteger(String.valueOf(len1)), new java.math.BigInteger(String.valueOf(len2))))));
        java.math.BigInteger insert_cost_1 = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(helper(word1, word2, ((java.math.BigInteger[][])(cache)), new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(j.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(len1)), new java.math.BigInteger(String.valueOf(len2))))));
        java.math.BigInteger replace_cost_1 = new java.math.BigInteger(String.valueOf(diff_1.add(helper(word1, word2, ((java.math.BigInteger[][])(cache)), new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(j.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(len1)), new java.math.BigInteger(String.valueOf(len2))))));
cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())][(int)(((java.math.BigInteger)(j)).longValue())] = new java.math.BigInteger(String.valueOf(min3(new java.math.BigInteger(String.valueOf(delete_cost_1)), new java.math.BigInteger(String.valueOf(insert_cost_1)), new java.math.BigInteger(String.valueOf(replace_cost_1)))));
        return new java.math.BigInteger(String.valueOf(cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())][_idx((cache[_idx((cache).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())]));
    }

    static java.math.BigInteger min_distance_up_bottom(String word1, String word2) {
        java.math.BigInteger len1 = new java.math.BigInteger(String.valueOf(_runeLen(word1)));
        java.math.BigInteger len2_1 = new java.math.BigInteger(String.valueOf(_runeLen(word2)));
        java.math.BigInteger[][] cache_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        for (java.math.BigInteger _v = new java.math.BigInteger(String.valueOf(0)); _v.compareTo(len1) < 0; _v = _v.add(java.math.BigInteger.ONE)) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            for (java.math.BigInteger _2 = new java.math.BigInteger(String.valueOf(0)); _2.compareTo(len2_1) < 0; _2 = _2.add(java.math.BigInteger.ONE)) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))).toArray(java.math.BigInteger[]::new)));
            }
            cache_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cache_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
        }
        return new java.math.BigInteger(String.valueOf(helper(word1, word2, ((java.math.BigInteger[][])(cache_1)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(len1)), new java.math.BigInteger(String.valueOf(len2_1)))));
    }
    public static void main(String[] args) {
        System.out.println(_p(min_distance_up_bottom("intention", "execution")));
        System.out.println(_p(min_distance_up_bottom("intention", "")));
        System.out.println(_p(min_distance_up_bottom("", "")));
        System.out.println(_p(min_distance_up_bottom("zooicoarchaeologist", "zoologist")));
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
