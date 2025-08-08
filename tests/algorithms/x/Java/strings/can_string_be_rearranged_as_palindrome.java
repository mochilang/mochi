public class Main {
    static String LETTERS;
    static String LOWERCASE;

    static String char_to_lower(String c) {
        int i = 0;
        while (i < _runeLen(LETTERS)) {
            if ((c.equals(_substr(LETTERS, i, i + 1)))) {
                return _substr(LOWERCASE, i, i + 1);
            }
            i = i + 1;
        }
        return c;
    }

    static String normalize(String input_str) {
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(input_str)) {
            String ch = _substr(input_str, i_1, i_1 + 1);
            String lc = String.valueOf(char_to_lower(ch));
            if ((lc.compareTo("a") >= 0) && (lc.compareTo("z") <= 0)) {
                res = res + lc;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static boolean can_string_be_rearranged_as_palindrome_counter(String input_str) {
        String s = String.valueOf(normalize(input_str));
        java.util.Map<String,Integer> freq = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String ch_1 = _substr(s, i_2, i_2 + 1);
            if (((Boolean)(freq.containsKey(ch_1)))) {
freq.put(ch_1, (int)(((int)(freq).getOrDefault(ch_1, 0))) + 1);
            } else {
freq.put(ch_1, 1);
            }
            i_2 = i_2 + 1;
        }
        int odd = 0;
        for (String key : freq.keySet()) {
            if (Math.floorMod(((int)(freq).getOrDefault(key, 0)), 2) != 0) {
                odd = odd + 1;
            }
        }
        return odd < 2;
    }

    static boolean can_string_be_rearranged_as_palindrome(String input_str) {
        String s_1 = String.valueOf(normalize(input_str));
        if (_runeLen(s_1) == 0) {
            return true;
        }
        java.util.Map<String,Integer> character_freq_dict = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_3 = 0;
        while (i_3 < _runeLen(s_1)) {
            String character = _substr(s_1, i_3, i_3 + 1);
            if (((Boolean)(character_freq_dict.containsKey(character)))) {
character_freq_dict.put(character, (int)(((int)(character_freq_dict).getOrDefault(character, 0))) + 1);
            } else {
character_freq_dict.put(character, 1);
            }
            i_3 = i_3 + 1;
        }
        int odd_char = 0;
        for (String character_key : character_freq_dict.keySet()) {
            int character_count = (int)(((int)(character_freq_dict).getOrDefault(character_key, 0)));
            if (Math.floorMod(character_count, 2) != 0) {
                odd_char = odd_char + 1;
            }
        }
        return !(odd_char > 1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
            System.out.println(can_string_be_rearranged_as_palindrome_counter("Momo"));
            System.out.println(can_string_be_rearranged_as_palindrome_counter("Mother"));
            System.out.println(can_string_be_rearranged_as_palindrome("Momo"));
            System.out.println(can_string_be_rearranged_as_palindrome("Mother"));
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
