public class Main {

    static boolean validate_initial_digits(String cc) {
        return (cc.substring(0, 2).equals("34")) || (cc.substring(0, 2).equals("35")) || (cc.substring(0, 2).equals("37")) || (cc.substring(0, 1).equals("4")) || (cc.substring(0, 1).equals("5")) || (cc.substring(0, 1).equals("6"));
    }

    static boolean luhn_validation(String cc) {
        int sum = 0;
        boolean double_digit = false;
        int i = _runeLen(cc) - 1;
        while (i >= 0) {
            int n = Integer.parseInt(cc.substring(i, i + 1));
            if (double_digit) {
                n = n * 2;
                if (n > 9) {
                    n = n - 9;
                }
            }
            sum = sum + n;
            double_digit = !double_digit;
            i = i - 1;
        }
        return Math.floorMod(sum, 10) == 0;
    }

    static boolean is_digit_string(String s) {
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String c = s.substring(i_1, i_1 + 1);
            if ((c.compareTo("0") < 0) || (c.compareTo("9") > 0)) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static boolean validate_credit_card_number(String cc) {
        String error_message = cc + " is an invalid credit card number because";
        if (!(Boolean)is_digit_string(cc)) {
            System.out.println(error_message + " it has nonnumerical characters.");
            return false;
        }
        if (!(_runeLen(cc) >= 13 && _runeLen(cc) <= 16)) {
            System.out.println(error_message + " of its length.");
            return false;
        }
        if (!(Boolean)validate_initial_digits(cc)) {
            System.out.println(error_message + " of its first two digits.");
            return false;
        }
        if (!(Boolean)luhn_validation(cc)) {
            System.out.println(error_message + " it fails the Luhn check.");
            return false;
        }
        System.out.println(cc + " is a valid credit card number.");
        return true;
    }

    static void main() {
        validate_credit_card_number("4111111111111111");
        validate_credit_card_number("32323");
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
