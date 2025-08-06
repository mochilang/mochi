public class Main {
    static String ALPHABET;
    static String message;
    static String key;
    static String key_new_1;
    static String encrypted;

    static int index_of(String ch) {
        for (int i = 0; i < _runeLen(ALPHABET); i++) {
            if ((ALPHABET.substring(i, i+1).equals(ch))) {
                return i;
            }
        }
        return -1;
    }

    static String generate_key(String message, String key) {
        String key_new = key;
        int i = 0;
        while (_runeLen(key_new) < _runeLen(message)) {
            key_new = key_new + key.substring(i, i+1);
            i = i + 1;
            if (i == _runeLen(key)) {
                i = 0;
            }
        }
        return key_new;
    }

    static String cipher_text(String message, String key_new) {
        String res = "";
        int i_1 = 0;
        for (int idx = 0; idx < _runeLen(message); idx++) {
            String ch = message.substring(idx, idx+1);
            if ((ch.equals(" "))) {
                res = res + " ";
            } else {
                int x = Math.floorMod((index_of(ch) - index_of(key_new.substring(i_1, i_1+1)) + 26), 26);
                i_1 = i_1 + 1;
                res = res + ALPHABET.substring(x, x+1);
            }
        }
        return res;
    }

    static String original_text(String cipher, String key_new) {
        String res_1 = "";
        int i_2 = 0;
        for (int idx = 0; idx < _runeLen(cipher); idx++) {
            String ch_1 = cipher.substring(idx, idx+1);
            if ((ch_1.equals(" "))) {
                res_1 = res_1 + " ";
            } else {
                int x_1 = Math.floorMod((index_of(ch_1) + index_of(key_new.substring(i_2, i_2+1)) + 26), 26);
                i_2 = i_2 + 1;
                res_1 = res_1 + ALPHABET.substring(x_1, x_1+1);
            }
        }
        return res_1;
    }
    public static void main(String[] args) {
        ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        message = "THE GERMAN ATTACK";
        key = "SECRET";
        key_new_1 = String.valueOf(generate_key(message, key));
        encrypted = String.valueOf(cipher_text(message, key_new_1));
        System.out.println("Encrypted Text = " + encrypted);
        System.out.println("Original Text = " + String.valueOf(original_text(encrypted, key_new_1)));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
