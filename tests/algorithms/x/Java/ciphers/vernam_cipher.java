public class Main {
    static String plaintext;
    static String key;
    static String encrypted;
    static String decrypted_1;

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int idx = indexOf(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        if (n >= 65 && n < 91) {
            return _substr(upper_1, n - 65, n - 64);
        }
        return "?";
    }

    static String vernam_encrypt(String plaintext, String key) {
        String ciphertext = "";
        int i_1 = 0;
        while (i_1 < _runeLen(plaintext)) {
            int p = ord(_substr(plaintext, i_1, i_1 + 1)) - 65;
            int k = ord(_substr(key, Math.floorMod(i_1, _runeLen(key)), Math.floorMod(i_1, _runeLen(key)) + 1)) - 65;
            int ct = p + k;
            while (ct > 25) {
                ct = ct - 26;
            }
            ciphertext = ciphertext + String.valueOf(chr(ct + 65));
            i_1 = i_1 + 1;
        }
        return ciphertext;
    }

    static String vernam_decrypt(String ciphertext, String key) {
        String decrypted = "";
        int i_2 = 0;
        while (i_2 < _runeLen(ciphertext)) {
            int c = ord(_substr(ciphertext, i_2, i_2 + 1));
            int k_1 = ord(_substr(key, Math.floorMod(i_2, _runeLen(key)), Math.floorMod(i_2, _runeLen(key)) + 1));
            int val = c - k_1;
            while (val < 0) {
                val = val + 26;
            }
            decrypted = decrypted + String.valueOf(chr(val + 65));
            i_2 = i_2 + 1;
        }
        return decrypted;
    }
    public static void main(String[] args) {
        plaintext = "HELLO";
        key = "KEY";
        encrypted = String.valueOf(vernam_encrypt(plaintext, key));
        decrypted_1 = String.valueOf(vernam_decrypt(encrypted, key));
        System.out.println("Plaintext: " + plaintext);
        System.out.println("Encrypted: " + encrypted);
        System.out.println("Decrypted: " + decrypted_1);
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
