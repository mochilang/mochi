public class Main {
    static String LETTERS_AND_SPACE;
    static String LOWER;
    static String UPPER;
    static java.util.Map<String,Boolean> ENGLISH_WORDS;

    static String to_upper(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i + 1);
            int j = 0;
            String up = c;
            while (j < _runeLen(LOWER)) {
                if ((c.equals(LOWER.substring(j, j + 1)))) {
                    up = UPPER.substring(j, j + 1);
                    break;
                }
                j = j + 1;
            }
            res = res + up;
            i = i + 1;
        }
        return res;
    }

    static boolean char_in(String chars, String c) {
        int i_1 = 0;
        while (i_1 < _runeLen(chars)) {
            if ((chars.substring(i_1, i_1 + 1).equals(c))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String remove_non_letters(String message) {
        String res_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String ch = message.substring(i_2, i_2 + 1);
            if (((Boolean)(char_in(LETTERS_AND_SPACE, ch)))) {
                res_1 = res_1 + ch;
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] split_spaces(String text) {
        String[] res_2 = ((String[])(new String[]{}));
        String current = "";
        int i_3 = 0;
        while (i_3 < _runeLen(text)) {
            String ch_1 = text.substring(i_3, i_3 + 1);
            if ((ch_1.equals(" "))) {
                res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch_1;
            }
            i_3 = i_3 + 1;
        }
        res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res_2;
    }

    static java.util.Map<String,Boolean> load_dictionary() {
        String[] words = ((String[])(new String[]{"HELLO", "WORLD", "HOW", "ARE", "YOU", "THE", "QUICK", "BROWN", "FOX", "JUMPS", "OVER", "LAZY", "DOG"}));
        java.util.Map<String,Boolean> dict = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (String w : words) {
dict.put(w, true);
        }
        return dict;
    }

    static double get_english_count(String message) {
        String upper = String.valueOf(to_upper(message));
        String cleaned = String.valueOf(remove_non_letters(upper));
        String[] possible = ((String[])(split_spaces(cleaned)));
        int matches = 0;
        int total = 0;
        for (String w : possible) {
            if (!(w.equals(""))) {
                total = total + 1;
                if (((Boolean)(ENGLISH_WORDS.containsKey(w)))) {
                    matches = matches + 1;
                }
            }
        }
        if (total == 0) {
            return 0.0;
        }
        return (((Number)(matches)).doubleValue()) / (((Number)(total)).doubleValue());
    }

    static boolean is_english(String message, int word_percentage, int letter_percentage) {
        boolean words_match = get_english_count(message) * 100.0 >= (((Number)(word_percentage)).doubleValue());
        int num_letters = String.valueOf(remove_non_letters(message)).length();
        double letters_pct = _runeLen(message) == 0 ? 0.0 : (((Number)(num_letters)).doubleValue()) / (((Number)(_runeLen(message))).doubleValue()) * 100.0;
        boolean letters_match = letters_pct >= (((Number)(letter_percentage)).doubleValue());
        return words_match && letters_match;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            LETTERS_AND_SPACE = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz \t\n";
            LOWER = "abcdefghijklmnopqrstuvwxyz";
            UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            ENGLISH_WORDS = load_dictionary();
            System.out.println(_p(is_english("Hello World", 20, 85)));
            System.out.println(_p(is_english("llold HorWd", 20, 85)));
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
