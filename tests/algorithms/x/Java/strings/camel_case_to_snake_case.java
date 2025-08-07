public class Main {
    static String LOWER;
    static String UPPER;
    static String DIGITS;

    static boolean is_lower(String ch) {
        int i = 0;
        while (i < _runeLen(LOWER)) {
            if ((LOWER.substring(i, i+1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean is_upper(String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(UPPER)) {
            if ((UPPER.substring(i_1, i_1+1).equals(ch))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static boolean is_digit(String ch) {
        int i_2 = 0;
        while (i_2 < _runeLen(DIGITS)) {
            if ((DIGITS.substring(i_2, i_2+1).equals(ch))) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static boolean is_alpha(String ch) {
        if (((Boolean)(is_lower(ch)))) {
            return true;
        }
        if (((Boolean)(is_upper(ch)))) {
            return true;
        }
        return false;
    }

    static boolean is_alnum(String ch) {
        if (((Boolean)(is_alpha(ch)))) {
            return true;
        }
        if (((Boolean)(is_digit(ch)))) {
            return true;
        }
        return false;
    }

    static String to_lower(String ch) {
        int i_3 = 0;
        while (i_3 < _runeLen(UPPER)) {
            if ((UPPER.substring(i_3, i_3+1).equals(ch))) {
                return LOWER.substring(i_3, i_3+1);
            }
            i_3 = i_3 + 1;
        }
        return ch;
    }

    static String camel_to_snake_case(String input_str) {
        String snake_str = "";
        int i_4 = 0;
        boolean prev_is_digit = false;
        boolean prev_is_alpha = false;
        while (i_4 < _runeLen(input_str)) {
            String ch = input_str.substring(i_4, i_4+1);
            if (((Boolean)(is_upper(ch)))) {
                snake_str = snake_str + "_" + String.valueOf(to_lower(ch));
            } else             if (prev_is_digit && ((Boolean)(is_lower(ch)))) {
                snake_str = snake_str + "_" + ch;
            } else             if (prev_is_alpha && ((Boolean)(is_digit(ch)))) {
                snake_str = snake_str + "_" + ch;
            } else             if (!(Boolean)is_alnum(ch)) {
                snake_str = snake_str + "_";
            } else {
                snake_str = snake_str + ch;
            }
            prev_is_digit = ((Boolean)(is_digit(ch)));
            prev_is_alpha = ((Boolean)(is_alpha(ch)));
            i_4 = i_4 + 1;
        }
        if (_runeLen(snake_str) > 0 && (snake_str.substring(0, 0+1).equals("_"))) {
            snake_str = snake_str.substring(1, _runeLen(snake_str));
        }
        return snake_str;
    }

    static void main() {
        System.out.println(camel_to_snake_case("someRandomString"));
        System.out.println(camel_to_snake_case("SomeRandomStr#ng"));
        System.out.println(camel_to_snake_case("123SomeRandom123String123"));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            LOWER = "abcdefghijklmnopqrstuvwxyz";
            UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            DIGITS = "0123456789";
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
