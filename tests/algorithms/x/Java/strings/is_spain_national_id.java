public class Main {
    static String DIGITS;
    static String UPPER;
    static String LOWER;
    static String LOOKUP_LETTERS;
    static String ERROR_MSG;

    static String to_upper(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i+1);
            int j = 0;
            String converted = ch;
            while (j < _runeLen(LOWER)) {
                if ((LOWER.substring(j, j+1).equals(ch))) {
                    converted = UPPER.substring(j, j+1);
                    break;
                }
                j = j + 1;
            }
            res = res + converted;
            i = i + 1;
        }
        return res;
    }

    static boolean is_digit(String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(DIGITS)) {
            if ((DIGITS.substring(i_1, i_1+1).equals(ch))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String clean_id(String spanish_id) {
        String upper_id = String.valueOf(to_upper(spanish_id));
        String cleaned = "";
        int i_2 = 0;
        while (i_2 < _runeLen(upper_id)) {
            String ch_1 = upper_id.substring(i_2, i_2+1);
            if (!(ch_1.equals("-"))) {
                cleaned = cleaned + ch_1;
            }
            i_2 = i_2 + 1;
        }
        return cleaned;
    }

    static boolean is_spain_national_id(String spanish_id) {
        String sid = String.valueOf(clean_id(spanish_id));
        if (_runeLen(sid) != 9) {
            throw new RuntimeException(String.valueOf(ERROR_MSG));
        }
        int i_3 = 0;
        while (i_3 < 8) {
            if (!(Boolean)is_digit(sid.substring(i_3, i_3+1))) {
                throw new RuntimeException(String.valueOf(ERROR_MSG));
            }
            i_3 = i_3 + 1;
        }
        int number = Integer.parseInt(_substr(sid, 0, 8));
        String letter = sid.substring(8, 8+1);
        if (((Boolean)(is_digit(letter)))) {
            throw new RuntimeException(String.valueOf(ERROR_MSG));
        }
        String expected = LOOKUP_LETTERS.substring(Math.floorMod(number, 23), Math.floorMod(number, 23)+1);
        return (letter.equals(expected));
    }

    static void main() {
        System.out.println(is_spain_national_id("12345678Z"));
        System.out.println(is_spain_national_id("12345678z"));
        System.out.println(is_spain_national_id("12345678x"));
        System.out.println(is_spain_national_id("12345678I"));
        System.out.println(is_spain_national_id("12345678-Z"));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            DIGITS = "0123456789";
            UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            LOWER = "abcdefghijklmnopqrstuvwxyz";
            LOOKUP_LETTERS = "TRWAGMYFPDXBNJZSQVHLCKE";
            ERROR_MSG = "Input must be a string of 8 numbers plus letter";
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
