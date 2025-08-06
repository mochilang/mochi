public class Main {
    static String key;
    static String plaintext;
    static String ciphertext;

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = indexOf(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = indexOf(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper_1.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower_1.substring(n - 97, n - 96);
        }
        return "?";
    }

    static String clean_text(String s) {
        String out = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = s.substring(i_1, i_1+1);
            if ((ch.compareTo("A") >= 0) && (ch.compareTo("Z") <= 0)) {
                out = out + ch;
            } else             if ((ch.compareTo("a") >= 0) && (ch.compareTo("z") <= 0)) {
                out = out + String.valueOf(chr(ord(ch) - 32));
            }
            i_1 = i_1 + 1;
        }
        return out;
    }

    static String running_key_encrypt(String key, String plaintext) {
        String pt = String.valueOf(clean_text(plaintext));
        String k = String.valueOf(clean_text(key));
        int key_len = _runeLen(k);
        String res = "";
        int ord_a = ord("A");
        int i_2 = 0;
        while (i_2 < _runeLen(pt)) {
            int p = ord(pt.substring(i_2, i_2+1)) - ord_a;
            int kv = ord(k.substring(Math.floorMod(i_2, key_len), Math.floorMod(i_2, key_len)+1)) - ord_a;
            int c = Math.floorMod((p + kv), 26);
            res = res + String.valueOf(chr(c + ord_a));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static String running_key_decrypt(String key, String ciphertext) {
        String ct = String.valueOf(clean_text(ciphertext));
        String k_1 = String.valueOf(clean_text(key));
        int key_len_1 = _runeLen(k_1);
        String res_1 = "";
        int ord_a_1 = ord("A");
        int i_3 = 0;
        while (i_3 < _runeLen(ct)) {
            int c_1 = ord(ct.substring(i_3, i_3+1)) - ord_a_1;
            int kv_1 = ord(k_1.substring(Math.floorMod(i_3, key_len_1), Math.floorMod(i_3, key_len_1)+1)) - ord_a_1;
            int p_1 = Math.floorMod((c_1 - kv_1 + 26), 26);
            res_1 = res_1 + String.valueOf(chr(p_1 + ord_a_1));
            i_3 = i_3 + 1;
        }
        return res_1;
    }
    public static void main(String[] args) {
        key = "How does the duck know that? said Victor";
        plaintext = "DEFEND THIS";
        ciphertext = String.valueOf(running_key_encrypt(key, plaintext));
        System.out.println(ciphertext);
        System.out.println(running_key_decrypt(key, ciphertext));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
