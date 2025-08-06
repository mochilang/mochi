public class Main {
    static String[] cipher_map;
    static String encoded;

    static int index_in_string(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static boolean contains_char(String s, String ch) {
        return index_in_string(s, ch) >= 0;
    }

    static boolean is_alpha(String ch) {
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        return ((Boolean)(contains_char(lower, ch))) || ((Boolean)(contains_char(upper, ch)));
    }

    static String to_upper(String s) {
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = s.substring(i_1, i_1+1);
            int idx = index_in_string(lower_1, ch);
            if (idx >= 0) {
                res = res + upper_1.substring(idx, idx+1);
            } else {
                res = res + ch;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String remove_duplicates(String key) {
        String res_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(key)) {
            String ch_1 = key.substring(i_2, i_2+1);
            if ((ch_1.equals(" ")) || (((Boolean)(is_alpha(ch_1))) && contains_char(res_1, ch_1) == false)) {
                res_1 = res_1 + ch_1;
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] create_cipher_map(String key) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String cleaned = String.valueOf(remove_duplicates(String.valueOf(to_upper(key))));
        String[] cipher = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < _runeLen(cleaned)) {
            cipher = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cipher), java.util.stream.Stream.of(cleaned.substring(i_3, i_3+1))).toArray(String[]::new)));
            i_3 = i_3 + 1;
        }
        int offset = _runeLen(cleaned);
        int j = cipher.length;
        while (j < 26) {
            String char_ = alphabet.substring(j - offset, j - offset+1);
            while (contains_char(cleaned, char_)) {
                offset = offset - 1;
                char_ = alphabet.substring(j - offset, j - offset+1);
            }
            cipher = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cipher), java.util.stream.Stream.of(char_)).toArray(String[]::new)));
            j = j + 1;
        }
        return cipher;
    }

    static int index_in_list(String[] lst, String ch) {
        int i_4 = 0;
        while (i_4 < lst.length) {
            if ((lst[i_4].equals(ch))) {
                return i_4;
            }
            i_4 = i_4 + 1;
        }
        return -1;
    }

    static String encipher(String message, String[] cipher) {
        String alphabet_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String msg = String.valueOf(to_upper(message));
        String res_2 = "";
        int i_5 = 0;
        while (i_5 < _runeLen(msg)) {
            String ch_2 = msg.substring(i_5, i_5+1);
            int idx_1 = index_in_string(alphabet_1, ch_2);
            if (idx_1 >= 0) {
                res_2 = res_2 + cipher[idx_1];
            } else {
                res_2 = res_2 + ch_2;
            }
            i_5 = i_5 + 1;
        }
        return res_2;
    }

    static String decipher(String message, String[] cipher) {
        String alphabet_2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String msg_1 = String.valueOf(to_upper(message));
        String res_3 = "";
        int i_6 = 0;
        while (i_6 < _runeLen(msg_1)) {
            String ch_3 = msg_1.substring(i_6, i_6+1);
            int idx_2 = index_in_list(((String[])(cipher)), ch_3);
            if (idx_2 >= 0) {
                res_3 = res_3 + alphabet_2.substring(idx_2, idx_2+1);
            } else {
                res_3 = res_3 + ch_3;
            }
            i_6 = i_6 + 1;
        }
        return res_3;
    }
    public static void main(String[] args) {
        cipher_map = ((String[])(create_cipher_map("Goodbye!!")));
        encoded = String.valueOf(encipher("Hello World!!", ((String[])(cipher_map))));
        System.out.println(encoded);
        System.out.println(decipher(encoded, ((String[])(cipher_map))));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
