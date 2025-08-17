public class Main {

    static void panic(String msg) {
    }

    static long char_to_value(String c) {
        String digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(digits))) {
            if ((digits.substring((int)((long)(i_1)), (int)((long)(i_1))+1).equals(c))) {
                return i_1;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        panic("invalid digit");
        return 0;
    }

    static String int_to_base(long number, long base) {
        if ((long)(base) < 2L || (long)(base) > 36L) {
            panic("'base' must be between 2 and 36 inclusive");
        }
        if ((long)(number) < 0L) {
            panic("number must be a positive integer");
        }
        String digits_2 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        long n_1 = (long)(number);
        String result_1 = "";
        while ((long)(n_1) > 0L) {
            long remainder_1 = Math.floorMod(n_1, base);
            result_1 = digits_2.substring((int)((long)(remainder_1)), (int)((long)(remainder_1))+1) + result_1;
            n_1 = Math.floorDiv(((long)(n_1)), ((long)(base)));
        }
        if ((result_1.equals(""))) {
            result_1 = "0";
        }
        return result_1;
    }

    static long base_to_int(String num_str, long base) {
        long value = 0L;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(num_str))) {
            String c_1 = num_str.substring((int)((long)(i_3)), (int)((long)(i_3))+1);
            value = (long)((long)((long)(value) * (long)(base)) + (long)(char_to_value(c_1)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return value;
    }

    static String sum_of_digits(long num, long base) {
        if ((long)(base) < 2L || (long)(base) > 36L) {
            panic("'base' must be between 2 and 36 inclusive");
        }
        String num_str_1 = String.valueOf(int_to_base((long)(num), (long)(base)));
        long total_1 = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(_runeLen(num_str_1))) {
            String c_3 = num_str_1.substring((int)((long)(i_5)), (int)((long)(i_5))+1);
            total_1 = (long)((long)(total_1) + (long)(char_to_value(c_3)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return int_to_base((long)(total_1), (long)(base));
    }

    static String[] harshad_numbers_in_base(long limit, long base) {
        if ((long)(base) < 2L || (long)(base) > 36L) {
            panic("'base' must be between 2 and 36 inclusive");
        }
        if ((long)(limit) < 0L) {
            return new String[]{};
        }
        String[] numbers_1 = ((String[])(new String[]{}));
        long i_7 = 1L;
        while ((long)(i_7) < (long)(limit)) {
            String s_1 = String.valueOf(sum_of_digits((long)(i_7), (long)(base)));
            long divisor_1 = (long)(base_to_int(s_1, (long)(base)));
            if (Math.floorMod(i_7, divisor_1) == 0L) {
                numbers_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers_1), java.util.stream.Stream.of(int_to_base((long)(i_7), (long)(base)))).toArray(String[]::new)));
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return numbers_1;
    }

    static boolean is_harshad_number_in_base(long num, long base) {
        if ((long)(base) < 2L || (long)(base) > 36L) {
            panic("'base' must be between 2 and 36 inclusive");
        }
        if ((long)(num) < 0L) {
            return false;
        }
        String n_3 = String.valueOf(int_to_base((long)(num), (long)(base)));
        String d_1 = String.valueOf(sum_of_digits((long)(num), (long)(base)));
        long n_val_1 = (long)(base_to_int(n_3, (long)(base)));
        long d_val_1 = (long)(base_to_int(d_1, (long)(base)));
        return Math.floorMod(n_val_1, d_val_1) == 0L;
    }

    static void main() {
        System.out.println(int_to_base(0L, 21L));
        System.out.println(int_to_base(23L, 2L));
        System.out.println(int_to_base(58L, 5L));
        System.out.println(int_to_base(167L, 16L));
        System.out.println(sum_of_digits(103L, 12L));
        System.out.println(sum_of_digits(1275L, 4L));
        System.out.println(sum_of_digits(6645L, 2L));
        System.out.println(harshad_numbers_in_base(15L, 2L));
        System.out.println(harshad_numbers_in_base(12L, 34L));
        System.out.println(harshad_numbers_in_base(12L, 4L));
        System.out.println(is_harshad_number_in_base(18L, 10L));
        System.out.println(is_harshad_number_in_base(21L, 10L));
        System.out.println(is_harshad_number_in_base((long)(-21), 5L));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
