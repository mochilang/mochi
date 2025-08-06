public class Main {
    static String LETTERS;
    static String LOWERCASE;
    static int seed = 0;
    static String key;

    static int rand(int n) {
        seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return Math.floorMod(seed, n);
    }

    static String get_random_key() {
        String[] chars = new String[0];
        int i = 0;
        while (i < _runeLen(LETTERS)) {
            chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(LETTERS.substring(i, i+1))).toArray(String[]::new)));
            i = i + 1;
        }
        int j = chars.length - 1;
        while (j > 0) {
            int k = rand(j + 1);
            String tmp = chars[j];
chars[j] = chars[k];
chars[k] = tmp;
            j = j - 1;
        }
        String res = "";
        i = 0;
        while (i < chars.length) {
            res = res + chars[i];
            i = i + 1;
        }
        return res;
    }

    static boolean check_valid_key(String key) {
        if (_runeLen(key) != _runeLen(LETTERS)) {
            return false;
        }
        java.util.Map<String,Boolean> used = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        int i_1 = 0;
        while (i_1 < _runeLen(key)) {
            String ch = key.substring(i_1, i_1+1);
            if (((boolean)(used).getOrDefault(ch, false))) {
                return false;
            }
used.put(ch, true);
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < _runeLen(LETTERS)) {
            String ch_1 = LETTERS.substring(i_1, i_1+1);
            if (!((boolean)(used).getOrDefault(ch_1, false))) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int index_in(String s, String ch) {
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            if ((s.substring(i_2, i_2+1).equals(ch))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return -1;
    }

    static String char_to_upper(String c) {
        int i_3 = 0;
        while (i_3 < _runeLen(LOWERCASE)) {
            if ((c.equals(LOWERCASE.substring(i_3, i_3+1)))) {
                return LETTERS.substring(i_3, i_3+1);
            }
            i_3 = i_3 + 1;
        }
        return c;
    }

    static String char_to_lower(String c) {
        int i_4 = 0;
        while (i_4 < _runeLen(LETTERS)) {
            if ((c.equals(LETTERS.substring(i_4, i_4+1)))) {
                return LOWERCASE.substring(i_4, i_4+1);
            }
            i_4 = i_4 + 1;
        }
        return c;
    }

    static boolean is_upper(String c) {
        int i_5 = 0;
        while (i_5 < _runeLen(LETTERS)) {
            if ((c.equals(LETTERS.substring(i_5, i_5+1)))) {
                return true;
            }
            i_5 = i_5 + 1;
        }
        return false;
    }

    static String translate_message(String key, String message, String mode) {
        String chars_a = LETTERS;
        String chars_b = key;
        if ((mode.equals("decrypt"))) {
            String tmp_1 = chars_a;
            chars_a = chars_b;
            chars_b = tmp_1;
        }
        String translated = "";
        int i_6 = 0;
        while (i_6 < _runeLen(message)) {
            String symbol = message.substring(i_6, i_6+1);
            String upper_symbol = String.valueOf(char_to_upper(symbol));
            int idx = index_in(chars_a, upper_symbol);
            if (idx >= 0) {
                String mapped = chars_b.substring(idx, idx+1);
                if (((Boolean)(is_upper(symbol)))) {
                    translated = translated + mapped;
                } else {
                    translated = translated + String.valueOf(char_to_lower(mapped));
                }
            } else {
                translated = translated + symbol;
            }
            i_6 = i_6 + 1;
        }
        return translated;
    }

    static String encrypt_message(String key, String message) {
        String res_1 = String.valueOf(translate_message(key, message, "encrypt"));
        return res_1;
    }

    static String decrypt_message(String key, String message) {
        String res_2 = String.valueOf(translate_message(key, message, "decrypt"));
        return res_2;
    }
    public static void main(String[] args) {
        LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
        seed = 1;
        key = "LFWOAYUISVKMNXPBDCRJTQEGHZ";
        System.out.println(encrypt_message(key, "Harshil Darji"));
        System.out.println(decrypt_message(key, "Ilcrism Olcvs"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
