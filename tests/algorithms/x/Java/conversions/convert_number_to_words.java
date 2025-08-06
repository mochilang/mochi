public class Main {
    static String[] ones;
    static String[] teens;
    static String[] tens;
    static int[] short_powers;
    static String[] short_units;
    static int[] long_powers;
    static String[] long_units;
    static int[] indian_powers;
    static String[] indian_units;

    static int pow10(int exp) {
        int res = 1;
        int i = 0;
        while (i < exp) {
            res = res * 10;
            i = i + 1;
        }
        return res;
    }

    static int max_value(String system) {
        if ((system.equals("short"))) {
            return pow10(18) - 1;
        }
        if ((system.equals("long"))) {
            return pow10(21) - 1;
        }
        if ((system.equals("indian"))) {
            return pow10(19) - 1;
        }
        return 0;
    }

    static String join_words(String[] words) {
        String res_1 = "";
        int i_1 = 0;
        while (i_1 < words.length) {
            if (i_1 > 0) {
                res_1 = res_1 + " ";
            }
            res_1 = res_1 + words[i_1];
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static String convert_small_number(int num) {
        if (num < 0) {
            return "";
        }
        if (num >= 100) {
            return "";
        }
        int tens_digit = num / 10;
        int ones_digit = Math.floorMod(num, 10);
        if (tens_digit == 0) {
            return ones[ones_digit];
        }
        if (tens_digit == 1) {
            return teens[ones_digit];
        }
        String hyphen = String.valueOf(ones_digit > 0 ? "-" : "");
        String tail = String.valueOf(ones_digit > 0 ? ones[ones_digit] : "");
        return tens[tens_digit] + hyphen + tail;
    }

    static String convert_number(int num, String system) {
        String[] word_groups = ((String[])(new String[]{}));
        int n = num;
        if (n < 0) {
            word_groups = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(word_groups), java.util.stream.Stream.of("negative")).toArray(String[]::new)));
            n = -n;
        }
        if (n > max_value(system)) {
            return "";
        }
        int[] powers = ((int[])(new int[]{}));
        String[] units = ((String[])(new String[]{}));
        if ((system.equals("short"))) {
            powers = ((int[])(short_powers));
            units = ((String[])(short_units));
        } else         if ((system.equals("long"))) {
            powers = ((int[])(long_powers));
            units = ((String[])(long_units));
        } else         if ((system.equals("indian"))) {
            powers = ((int[])(indian_powers));
            units = ((String[])(indian_units));
        } else {
            return "";
        }
        int i_2 = 0;
        while (i_2 < powers.length) {
            int power = powers[i_2];
            String unit = units[i_2];
            int divisor = pow10(power);
            int digit_group = n / divisor;
            n = Math.floorMod(n, divisor);
            if (digit_group > 0) {
                String word_group = String.valueOf(digit_group >= 100 ? convert_number(digit_group, system) : convert_small_number(digit_group));
                word_groups = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(word_groups), java.util.stream.Stream.of(word_group + " " + unit)).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        if (n > 0 || word_groups.length == 0) {
            word_groups = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(word_groups), java.util.stream.Stream.of(convert_small_number(n))).toArray(String[]::new)));
        }
        String joined = String.valueOf(join_words(((String[])(word_groups))));
        return joined;
    }
    public static void main(String[] args) {
        ones = ((String[])(new String[]{"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}));
        teens = ((String[])(new String[]{"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}));
        tens = ((String[])(new String[]{"", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}));
        short_powers = ((int[])(new int[]{15, 12, 9, 6, 3, 2}));
        short_units = ((String[])(new String[]{"quadrillion", "trillion", "billion", "million", "thousand", "hundred"}));
        long_powers = ((int[])(new int[]{15, 9, 6, 3, 2}));
        long_units = ((String[])(new String[]{"billiard", "milliard", "million", "thousand", "hundred"}));
        indian_powers = ((int[])(new int[]{14, 12, 7, 5, 3, 2}));
        indian_units = ((String[])(new String[]{"crore crore", "lakh crore", "crore", "lakh", "thousand", "hundred"}));
        System.out.println(convert_number((int)123456789012345L, "short"));
        System.out.println(convert_number((int)123456789012345L, "long"));
        System.out.println(convert_number((int)123456789012345L, "indian"));
    }
}
