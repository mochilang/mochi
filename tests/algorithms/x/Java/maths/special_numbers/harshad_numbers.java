public class Main {

    static void panic(String msg) {
    }

    static int char_to_value(String c) {
        String digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int i = 0;
        while (i < _runeLen(digits)) {
            if ((digits.substring(i, i+1).equals(c))) {
                return i;
            }
            i = i + 1;
        }
        throw new RuntimeException(String.valueOf("invalid digit"));
    }

    static String int_to_base(int number, int base) {
        if (base < 2 || base > 36) {
            throw new RuntimeException(String.valueOf("'base' must be between 2 and 36 inclusive"));
        }
        if (number < 0) {
            throw new RuntimeException(String.valueOf("number must be a positive integer"));
        }
        String digits_1 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int n = number;
        String result = "";
        while (n > 0) {
            int remainder = Math.floorMod(n, base);
            result = digits_1.substring(remainder, remainder+1) + result;
            n = Math.floorDiv(n, base);
        }
        if ((result.equals(""))) {
            result = "0";
        }
        return result;
    }

    static int base_to_int(String num_str, int base) {
        int value = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(num_str)) {
            String c = num_str.substring(i_1, i_1+1);
            value = value * base + char_to_value(c);
            i_1 = i_1 + 1;
        }
        return value;
    }

    static String sum_of_digits(int num, int base) {
        if (base < 2 || base > 36) {
            throw new RuntimeException(String.valueOf("'base' must be between 2 and 36 inclusive"));
        }
        String num_str = String.valueOf(int_to_base(num, base));
        int total = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(num_str)) {
            String c_1 = num_str.substring(i_2, i_2+1);
            total = total + char_to_value(c_1);
            i_2 = i_2 + 1;
        }
        return int_to_base(total, base);
    }

    static String[] harshad_numbers_in_base(int limit, int base) {
        if (base < 2 || base > 36) {
            throw new RuntimeException(String.valueOf("'base' must be between 2 and 36 inclusive"));
        }
        if (limit < 0) {
            return new String[]{};
        }
        String[] numbers = ((String[])(new String[]{}));
        int i_3 = 1;
        while (i_3 < limit) {
            String s = String.valueOf(sum_of_digits(i_3, base));
            int divisor = base_to_int(s, base);
            if (Math.floorMod(i_3, divisor) == 0) {
                numbers = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers), java.util.stream.Stream.of(int_to_base(i_3, base))).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return numbers;
    }

    static boolean is_harshad_number_in_base(int num, int base) {
        if (base < 2 || base > 36) {
            throw new RuntimeException(String.valueOf("'base' must be between 2 and 36 inclusive"));
        }
        if (num < 0) {
            return false;
        }
        String n_1 = String.valueOf(int_to_base(num, base));
        String d = String.valueOf(sum_of_digits(num, base));
        int n_val = base_to_int(n_1, base);
        int d_val = base_to_int(d, base);
        return Math.floorMod(n_val, d_val) == 0;
    }

    static void main() {
        System.out.println(int_to_base(0, 21));
        System.out.println(int_to_base(23, 2));
        System.out.println(int_to_base(58, 5));
        System.out.println(int_to_base(167, 16));
        System.out.println(sum_of_digits(103, 12));
        System.out.println(sum_of_digits(1275, 4));
        System.out.println(sum_of_digits(6645, 2));
        System.out.println(harshad_numbers_in_base(15, 2));
        System.out.println(harshad_numbers_in_base(12, 34));
        System.out.println(harshad_numbers_in_base(12, 4));
        System.out.println(is_harshad_number_in_base(18, 10));
        System.out.println(is_harshad_number_in_base(21, 10));
        System.out.println(is_harshad_number_in_base(-21, 5));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
