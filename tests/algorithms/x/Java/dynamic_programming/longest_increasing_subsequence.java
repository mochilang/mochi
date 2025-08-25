public class Main {
    static java.math.BigInteger[] longest_subsequence(java.math.BigInteger[] xs) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(xs.length));
        if (n.compareTo(java.math.BigInteger.valueOf(1)) <= 0) {
            return ((java.math.BigInteger[])(xs));
        }
        java.math.BigInteger pivot_1 = new java.math.BigInteger(String.valueOf(xs[_idx((xs).length, 0L)]));
        boolean is_found_1 = false;
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        java.math.BigInteger[] longest_subseq_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        while (!is_found_1 && i_1.compareTo(n) < 0) {
            if (xs[_idx((xs).length, ((java.math.BigInteger)(i_1)).longValue())].compareTo(pivot_1) < 0) {
                is_found_1 = true;
                java.math.BigInteger[] temp_array_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(xs, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(n)).longValue()))));
                temp_array_1 = ((java.math.BigInteger[])(longest_subsequence(((java.math.BigInteger[])(temp_array_1)))));
                if (new java.math.BigInteger(String.valueOf(temp_array_1.length)).compareTo(new java.math.BigInteger(String.valueOf(longest_subseq_1.length))) > 0) {
                    longest_subseq_1 = ((java.math.BigInteger[])(temp_array_1));
                }
            } else {
                i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        java.math.BigInteger[] filtered_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(n) < 0) {
            if (xs[_idx((xs).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(pivot_1) >= 0) {
                filtered_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(filtered_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(xs[_idx((xs).length, ((java.math.BigInteger)(j_1)).longValue())])))).toArray(java.math.BigInteger[]::new)));
            }
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] candidate_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        candidate_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(candidate_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(pivot_1)))).toArray(java.math.BigInteger[]::new)));
        candidate_1 = ((java.math.BigInteger[])(concat(candidate_1, longest_subsequence(((java.math.BigInteger[])(filtered_1))))));
        if (new java.math.BigInteger(String.valueOf(candidate_1.length)).compareTo(new java.math.BigInteger(String.valueOf(longest_subseq_1.length))) > 0) {
            return ((java.math.BigInteger[])(candidate_1));
        } else {
            return ((java.math.BigInteger[])(longest_subseq_1));
        }
    }
    public static void main(String[] args) {
    }

    static Object concat(Object a, Object b) {
        int len1 = java.lang.reflect.Array.getLength(a);
        int len2 = java.lang.reflect.Array.getLength(b);
        Object out = java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), len1 + len2);
        System.arraycopy(a, 0, out, 0, len1);
        System.arraycopy(b, 0, out, len1, len2);
        return out;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
