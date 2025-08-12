public class Main {
    static String ASCII;
    static String message;
    static long token;
    static String encoded;

    static String[] build_alphabet() {
        String[] result = ((String[])(new String[]{}));
        long i_1 = 0L;
        while (i_1 < _runeLen(ASCII)) {
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(ASCII.substring((int)((long)(i_1)), (int)((long)(i_1))+1))).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static long[] range_list(long n) {
        long[] lst = ((long[])(new long[]{}));
        long i_3 = 0L;
        while (i_3 < n) {
            lst = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lst), java.util.stream.LongStream.of(i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        return lst;
    }

    static long[] reversed_range_list(long n) {
        long[] lst_1 = ((long[])(new long[]{}));
        long i_5 = n - 1;
        while (i_5 >= 0) {
            lst_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lst_1), java.util.stream.LongStream.of(i_5)).toArray()));
            i_5 = i_5 - 1;
        }
        return lst_1;
    }

    static long index_of_char(String[] lst, String ch) {
        long i_6 = 0L;
        while (i_6 < lst.length) {
            if ((lst[(int)((long)(i_6))].equals(ch))) {
                return i_6;
            }
            i_6 = i_6 + 1;
        }
        return -1;
    }

    static long index_of_int(long[] lst, long value) {
        long i_7 = 0L;
        while (i_7 < lst.length) {
            if (lst[(int)((long)(i_7))] == value) {
                return i_7;
            }
            i_7 = i_7 + 1;
        }
        return -1;
    }

    static String enigma_encrypt(String message, long token) {
        String[] alphabets = ((String[])(build_alphabet()));
        long n_1 = alphabets.length;
        long[][] gear_one_1 = new long[1][];
        gear_one_1[0] = ((long[])(range_list(n_1)));
        long[][] gear_two_1 = new long[1][];
        gear_two_1[0] = ((long[])(range_list(n_1)));
        long[][] gear_three_1 = new long[1][];
        gear_three_1[0] = ((long[])(range_list(n_1)));
        long[] reflector_1 = ((long[])(reversed_range_list(n_1)));
        long[] gear_one_pos_1 = new long[1];
        gear_one_pos_1[0] = 0;
        long[] gear_two_pos_1 = new long[1];
        gear_two_pos_1[0] = 0;
        long[] gear_three_pos_1 = new long[1];
        gear_three_pos_1[0] = 0;
        Runnable[] rotator = new Runnable[1];
        rotator[0] = () -> {
        long i_9 = gear_one_1[0][(int)((long)(0))];
        gear_one_1[0] = ((long[])(java.util.Arrays.copyOfRange(gear_one_1[0], (int)((long)(1)), (int)((long)(gear_one_1[0].length)))));
        gear_one_1[0] = ((long[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_one_1[0]), java.util.stream.IntStream.of(i_9)).toArray()));
        gear_one_pos_1[0] = gear_one_pos_1[0] + 1;
        if (Math.floorMod(gear_one_pos_1[0], n_1) == 0) {
            i_9 = gear_two_1[0][(int)((long)(0))];
            gear_two_1[0] = ((long[])(java.util.Arrays.copyOfRange(gear_two_1[0], (int)((long)(1)), (int)((long)(gear_two_1[0].length)))));
            gear_two_1[0] = ((long[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_two_1[0]), java.util.stream.IntStream.of(i_9)).toArray()));
            gear_two_pos_1[0] = gear_two_pos_1[0] + 1;
            if (Math.floorMod(gear_two_pos_1[0], n_1) == 0) {
                i_9 = gear_three_1[0][(int)((long)(0))];
                gear_three_1[0] = ((long[])(java.util.Arrays.copyOfRange(gear_three_1[0], (int)((long)(1)), (int)((long)(gear_three_1[0].length)))));
                gear_three_1[0] = ((long[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(gear_three_1[0]), java.util.stream.IntStream.of(i_9)).toArray()));
                gear_three_pos_1[0] = gear_three_pos_1[0] + 1;
            }
        }
};
        java.util.function.Function<String,String>[] engine = new java.util.function.Function[1];
        engine[0] = (ch_1) -> {
        long target_1 = index_of_char(((String[])(alphabets)), ch_1);
        target_1 = gear_one_1[0][(int)((long)(target_1))];
        target_1 = gear_two_1[0][(int)((long)(target_1))];
        target_1 = gear_three_1[0][(int)((long)(target_1))];
        target_1 = reflector_1[(int)((long)(target_1))];
        target_1 = index_of_int(((long[])(gear_three_1[0])), target_1);
        target_1 = index_of_int(((long[])(gear_two_1[0])), target_1);
        target_1 = index_of_int(((long[])(gear_one_1[0])), target_1);
        rotator[0].run();
        return alphabets[(int)((long)(target_1))];
};
        long t_1 = 0L;
        while (t_1 < token) {
            rotator[0].run();
            t_1 = t_1 + 1;
        }
        String result_2 = "";
        long idx_1 = 0L;
        while (idx_1 < _runeLen(message)) {
            result_2 = result_2 + String.valueOf(engine[0].apply(message.substring((int)((long)(idx_1)), (int)((long)(idx_1))+1)));
            idx_1 = idx_1 + 1;
        }
        return result_2;
    }
    public static void main(String[] args) {
        ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}";
        message = "HELLO WORLD";
        token = 123;
        encoded = String.valueOf(enigma_encrypt(message, token));
        System.out.println(encoded);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
