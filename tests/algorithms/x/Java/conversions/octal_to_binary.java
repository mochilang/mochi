public class Main {

    static String octal_to_binary(String octal_number) {
        if (_runeLen(octal_number) == 0) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
        }
        String octal_digits = "01234567";
        String binary_number = "";
        int i = 0;
        while (i < _runeLen(octal_number)) {
            String digit = octal_number.substring(i, i+1);
            boolean valid = false;
            int j = 0;
            while (j < _runeLen(octal_digits)) {
                if ((digit.equals(octal_digits.substring(j, j+1)))) {
                    valid = true;
                    break;
                }
                j = j + 1;
            }
            if (!valid) {
                throw new RuntimeException(String.valueOf("Non-octal value was passed to the function"));
            }
            int value = Integer.parseInt(digit);
            int k = 0;
            String binary_digit = "";
            while (k < 3) {
                binary_digit = _p(Math.floorMod(value, 2)) + binary_digit;
                value = value / 2;
                k = k + 1;
            }
            binary_number = binary_number + binary_digit;
            i = i + 1;
        }
        return binary_number;
    }
    public static void main(String[] args) {
        System.out.println(octal_to_binary("17"));
        System.out.println(octal_to_binary("7"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
        return String.valueOf(v);
    }
}
