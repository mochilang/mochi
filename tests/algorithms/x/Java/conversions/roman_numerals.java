public class Main {
    static int[] roman_values;
    static String[] roman_symbols;

    static int char_value(String c) {
        if ((c.equals("I"))) {
            return 1;
        }
        if ((c.equals("V"))) {
            return 5;
        }
        if ((c.equals("X"))) {
            return 10;
        }
        if ((c.equals("L"))) {
            return 50;
        }
        if ((c.equals("C"))) {
            return 100;
        }
        if ((c.equals("D"))) {
            return 500;
        }
        if ((c.equals("M"))) {
            return 1000;
        }
        return 0;
    }

    static int roman_to_int(String roman) {
        int total = 0;
        int i = 0;
        while (i < _runeLen(roman)) {
            if (i + 1 < _runeLen(roman) && char_value(roman.substring(i, i+1)) < char_value(roman.substring(i + 1, i + 1+1))) {
                total = total + char_value(roman.substring(i + 1, i + 1+1)) - char_value(roman.substring(i, i+1));
                i = i + 2;
            } else {
                total = total + char_value(roman.substring(i, i+1));
                i = i + 1;
            }
        }
        return total;
    }

    static String int_to_roman(int number) {
        int num = number;
        String res = "";
        int i_1 = 0;
        while (i_1 < roman_values.length) {
            int value = roman_values[i_1];
            String symbol = roman_symbols[i_1];
            int factor = num / value;
            num = Math.floorMod(num, value);
            int j = 0;
            while (j < factor) {
                res = res + symbol;
                j = j + 1;
            }
            if (num == 0) {
                break;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        roman_values = ((int[])(new int[]{1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1}));
        roman_symbols = ((String[])(new String[]{"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"}));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
