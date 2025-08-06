public class Main {
    static String default_alphabet;

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String encrypt(String input_string, int key, String alphabet) {
        String result = "";
        int i_1 = 0;
        int n = _runeLen(alphabet);
        while (i_1 < _runeLen(input_string)) {
            String ch = _substr(input_string, i_1, i_1 + 1);
            int idx = index_of(alphabet, ch);
            if (idx < 0) {
                result = result + ch;
            } else {
                int new_key = Math.floorMod((idx + key), n);
                if (new_key < 0) {
                    new_key = new_key + n;
                }
                result = result + _substr(alphabet, new_key, new_key + 1);
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String decrypt(String input_string, int key, String alphabet) {
        String result_1 = "";
        int i_2 = 0;
        int n_1 = _runeLen(alphabet);
        while (i_2 < _runeLen(input_string)) {
            String ch_1 = _substr(input_string, i_2, i_2 + 1);
            int idx_1 = index_of(alphabet, ch_1);
            if (idx_1 < 0) {
                result_1 = result_1 + ch_1;
            } else {
                int new_key_1 = Math.floorMod((idx_1 - key), n_1);
                if (new_key_1 < 0) {
                    new_key_1 = new_key_1 + n_1;
                }
                result_1 = result_1 + _substr(alphabet, new_key_1, new_key_1 + 1);
            }
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static String[] brute_force(String input_string, String alphabet) {
        String[] results = ((String[])(new String[]{}));
        int key = 1;
        int n_2 = _runeLen(alphabet);
        while (key <= n_2) {
            String message = String.valueOf(decrypt(input_string, key, alphabet));
            results = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(results), java.util.stream.Stream.of(message)).toArray(String[]::new)));
            key = key + 1;
        }
        return results;
    }

    static void main() {
        String alpha = default_alphabet;
        String enc = String.valueOf(encrypt("The quick brown fox jumps over the lazy dog", 8, alpha));
        System.out.println(enc);
        String dec = String.valueOf(decrypt(enc, 8, alpha));
        System.out.println(dec);
        String[] brute = ((String[])(brute_force("jFyuMy xIH'N vLONy zILwy Gy!", alpha)));
        System.out.println(brute[19]);
    }
    public static void main(String[] args) {
        default_alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
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
