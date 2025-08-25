public class Main {

    static java.math.BigInteger ceil_index(java.math.BigInteger[] v, java.math.BigInteger left, java.math.BigInteger right, java.math.BigInteger key) {
        java.math.BigInteger l = new java.math.BigInteger(String.valueOf(left));
        java.math.BigInteger r_1 = new java.math.BigInteger(String.valueOf(right));
        while (r_1.subtract(l).compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            java.math.BigInteger middle_1 = new java.math.BigInteger(String.valueOf((l.add(r_1)).divide(java.math.BigInteger.valueOf(2))));
            if (v[_idx((v).length, ((java.math.BigInteger)(middle_1)).longValue())].compareTo(key) >= 0) {
                r_1 = new java.math.BigInteger(String.valueOf(middle_1));
            } else {
                l = new java.math.BigInteger(String.valueOf(middle_1));
            }
        }
        return new java.math.BigInteger(String.valueOf(r_1));
    }

    static java.math.BigInteger longest_increasing_subsequence_length(java.math.BigInteger[] v) {
        if (new java.math.BigInteger(String.valueOf(v.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger[] tail_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
            tail_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tail_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger length_1 = java.math.BigInteger.valueOf(1);
tail_1[(int)(0L)] = new java.math.BigInteger(String.valueOf(v[_idx((v).length, 0L)]));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(new java.math.BigInteger(String.valueOf(v.length))) < 0) {
            if (v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(tail_1[_idx((tail_1).length, 0L)]) < 0) {
tail_1[(int)(0L)] = new java.math.BigInteger(String.valueOf(v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())]));
            } else             if (v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(tail_1[_idx((tail_1).length, ((java.math.BigInteger)(length_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) > 0) {
tail_1[(int)(((java.math.BigInteger)(length_1)).longValue())] = new java.math.BigInteger(String.valueOf(v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())]));
                length_1 = new java.math.BigInteger(String.valueOf(length_1.add(java.math.BigInteger.valueOf(1))));
            } else {
                java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(ceil_index(((java.math.BigInteger[])(tail_1)), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf(length_1.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())])))));
tail_1[(int)(((java.math.BigInteger)(idx_1)).longValue())] = new java.math.BigInteger(String.valueOf(v[_idx((v).length, ((java.math.BigInteger)(j_1)).longValue())]));
            }
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(length_1));
    }

    static void main() {
        java.math.BigInteger[] example1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(11), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(6)}));
        java.math.BigInteger[] example2_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] example3_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(11), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(15)}));
        java.math.BigInteger[] example4_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)}));
        System.out.println(longest_increasing_subsequence_length(((java.math.BigInteger[])(example1))));
        System.out.println(longest_increasing_subsequence_length(((java.math.BigInteger[])(example2_1))));
        System.out.println(longest_increasing_subsequence_length(((java.math.BigInteger[])(example3_1))));
        System.out.println(longest_increasing_subsequence_length(((java.math.BigInteger[])(example4_1))));
    }
    public static void main(String[] args) {
        main();
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
