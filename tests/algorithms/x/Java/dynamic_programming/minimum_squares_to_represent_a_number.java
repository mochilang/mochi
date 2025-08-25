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

    static java.math.BigInteger int_sqrt(java.math.BigInteger n) {
        java.math.BigInteger r = java.math.BigInteger.valueOf(0);
        while ((r.add(java.math.BigInteger.valueOf(1))).multiply((r.add(java.math.BigInteger.valueOf(1)))).compareTo(n) <= 0) {
            r = new java.math.BigInteger(String.valueOf(r.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(r));
    }

    static java.math.BigInteger minimum_squares_to_represent_a_number(java.math.BigInteger number) {
        if (number.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            throw new RuntimeException(String.valueOf("the value of input must not be a negative number"));
        }
        if (number.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(1);
        }
        java.math.BigInteger[] answers_1 = ((java.math.BigInteger[])(make_list(new java.math.BigInteger(String.valueOf(number.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))));
answers_1[(int)(0L)] = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(number) <= 0) {
            java.math.BigInteger answer_1 = new java.math.BigInteger(String.valueOf(i_3));
            java.math.BigInteger root_1 = new java.math.BigInteger(String.valueOf(int_sqrt(new java.math.BigInteger(String.valueOf(i_3)))));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
            while (j_1.compareTo(root_1) <= 0) {
                java.math.BigInteger current_answer_1 = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(answers_1[_idx((answers_1).length, ((java.math.BigInteger)(i_3.subtract(j_1.multiply(j_1)))).longValue())])));
                if (current_answer_1.compareTo(answer_1) < 0) {
                    answer_1 = new java.math.BigInteger(String.valueOf(current_answer_1));
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
answers_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(answer_1));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(answers_1[_idx((answers_1).length, ((java.math.BigInteger)(number)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(minimum_squares_to_represent_a_number(java.math.BigInteger.valueOf(25)));
        System.out.println(minimum_squares_to_represent_a_number(java.math.BigInteger.valueOf(21)));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
