public class Main {
    static String[] triagrams;

    static String remove_spaces(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String c = _substr(s, i, i + 1);
            if (!(c.equals(" "))) {
                res = res + c;
            }
            i = i + 1;
        }
        return res;
    }

    static String char_to_trigram(String ch, String alphabet) {
        int i_1 = 0;
        while (i_1 < _runeLen(alphabet)) {
            if ((_substr(alphabet, i_1, i_1 + 1).equals(ch))) {
                return triagrams[i_1];
            }
            i_1 = i_1 + 1;
        }
        return "";
    }

    static String trigram_to_char(String tri, String alphabet) {
        int i_2 = 0;
        while (i_2 < triagrams.length) {
            if ((triagrams[i_2].equals(tri))) {
                return _substr(alphabet, i_2, i_2 + 1);
            }
            i_2 = i_2 + 1;
        }
        return "";
    }

    static String encrypt_part(String part, String alphabet) {
        String one = "";
        String two = "";
        String three = "";
        int i_3 = 0;
        while (i_3 < _runeLen(part)) {
            String tri = String.valueOf(char_to_trigram(_substr(part, i_3, i_3 + 1), alphabet));
            one = one + _substr(tri, 0, 1);
            two = two + _substr(tri, 1, 2);
            three = three + _substr(tri, 2, 3);
            i_3 = i_3 + 1;
        }
        return one + two + three;
    }

    static String encrypt_message(String message, String alphabet, int period) {
        String msg = String.valueOf(remove_spaces(message));
        String alpha = String.valueOf(remove_spaces(alphabet));
        if (_runeLen(alpha) != 27) {
            return "";
        }
        String encrypted_numeric = "";
        int i_4 = 0;
        while (i_4 < _runeLen(msg)) {
            int end = i_4 + period;
            if (end > _runeLen(msg)) {
                end = _runeLen(msg);
            }
            String part = _substr(msg, i_4, end);
            encrypted_numeric = encrypted_numeric + String.valueOf(encrypt_part(part, alpha));
            i_4 = i_4 + period;
        }
        String encrypted = "";
        int j = 0;
        while (j < _runeLen(encrypted_numeric)) {
            String tri_1 = _substr(encrypted_numeric, j, j + 3);
            encrypted = encrypted + String.valueOf(trigram_to_char(tri_1, alpha));
            j = j + 3;
        }
        return encrypted;
    }

    static String[] decrypt_part(String part, String alphabet) {
        String converted = "";
        int i_5 = 0;
        while (i_5 < _runeLen(part)) {
            String tri_2 = String.valueOf(char_to_trigram(_substr(part, i_5, i_5 + 1), alphabet));
            converted = converted + tri_2;
            i_5 = i_5 + 1;
        }
        String[] result = ((String[])(new String[]{}));
        String tmp = "";
        int j_1 = 0;
        while (j_1 < _runeLen(converted)) {
            tmp = tmp + _substr(converted, j_1, j_1 + 1);
            if (_runeLen(tmp) == _runeLen(part)) {
                result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(tmp)).toArray(String[]::new)));
                tmp = "";
            }
            j_1 = j_1 + 1;
        }
        return result;
    }

    static String decrypt_message(String message, String alphabet, int period) {
        String msg_1 = String.valueOf(remove_spaces(message));
        String alpha_1 = String.valueOf(remove_spaces(alphabet));
        if (_runeLen(alpha_1) != 27) {
            return "";
        }
        String[] decrypted_numeric = ((String[])(new String[]{}));
        int i_6 = 0;
        while (i_6 < _runeLen(msg_1)) {
            int end_1 = i_6 + period;
            if (end_1 > _runeLen(msg_1)) {
                end_1 = _runeLen(msg_1);
            }
            String part_1 = _substr(msg_1, i_6, end_1);
            String[] groups = ((String[])(decrypt_part(part_1, alpha_1)));
            int k = 0;
            while (k < groups[0].length()) {
                String tri_3 = _substr(groups[0], k, k + 1) + _substr(groups[1], k, k + 1) + _substr(groups[2], k, k + 1);
                decrypted_numeric = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(decrypted_numeric), java.util.stream.Stream.of(tri_3)).toArray(String[]::new)));
                k = k + 1;
            }
            i_6 = i_6 + period;
        }
        String decrypted = "";
        int j_2 = 0;
        while (j_2 < decrypted_numeric.length) {
            decrypted = decrypted + String.valueOf(trigram_to_char(decrypted_numeric[j_2], alpha_1));
            j_2 = j_2 + 1;
        }
        return decrypted;
    }

    static void main() {
        String msg_2 = "DEFEND THE EAST WALL OF THE CASTLE.";
        String alphabet = "EPSDUCVWYM.ZLKXNBTFGORIJHAQ";
        String encrypted_1 = String.valueOf(encrypt_message(msg_2, alphabet, 5));
        String decrypted_1 = String.valueOf(decrypt_message(encrypted_1, alphabet, 5));
        System.out.println("Encrypted: " + encrypted_1);
        System.out.println("Decrypted: " + decrypted_1);
    }
    public static void main(String[] args) {
        triagrams = ((String[])(new String[]{"111", "112", "113", "121", "122", "123", "131", "132", "133", "211", "212", "213", "221", "222", "223", "231", "232", "233", "311", "312", "313", "321", "322", "323", "331", "332", "333"}));
        main();
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
