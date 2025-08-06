public class Main {
    static String LOWER;
    static String UPPER;

    static String to_lowercase(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            int j = 0;
            boolean found = false;
            while (j < 26) {
                if ((c.equals(UPPER.substring(j, j+1)))) {
                    res = res + LOWER.substring(j, j+1);
                    found = true;
                    break;
                }
                j = j + 1;
            }
            if (!found) {
                res = res + c;
            }
            i = i + 1;
        }
        return res;
    }

    static int char_index(String c) {
        int i_1 = 0;
        while (i_1 < 26) {
            if ((c.equals(LOWER.substring(i_1, i_1+1)))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String index_char(int i) {
        return LOWER.substring(i, i+1);
    }

    static String encrypt(String plaintext, String key) {
        if (_runeLen(plaintext) == 0) {
            throw new RuntimeException(String.valueOf("plaintext is empty"));
        }
        if (_runeLen(key) == 0) {
            throw new RuntimeException(String.valueOf("key is empty"));
        }
        String full_key = key + plaintext;
        plaintext = String.valueOf(to_lowercase(plaintext));
        full_key = String.valueOf(to_lowercase(full_key));
        int p_i = 0;
        int k_i = 0;
        String ciphertext = "";
        while (p_i < _runeLen(plaintext)) {
            String p_char = plaintext.substring(p_i, p_i+1);
            int p_idx = char_index(p_char);
            if (p_idx < 0) {
                ciphertext = ciphertext + p_char;
                p_i = p_i + 1;
            } else {
                String k_char = full_key.substring(k_i, k_i+1);
                int k_idx = char_index(k_char);
                if (k_idx < 0) {
                    k_i = k_i + 1;
                } else {
                    int c_idx = Math.floorMod((p_idx + k_idx), 26);
                    ciphertext = ciphertext + String.valueOf(index_char(c_idx));
                    k_i = k_i + 1;
                    p_i = p_i + 1;
                }
            }
        }
        return ciphertext;
    }

    static String decrypt(String ciphertext, String key) {
        if (_runeLen(ciphertext) == 0) {
            throw new RuntimeException(String.valueOf("ciphertext is empty"));
        }
        if (_runeLen(key) == 0) {
            throw new RuntimeException(String.valueOf("key is empty"));
        }
        String current_key = String.valueOf(to_lowercase(key));
        int c_i = 0;
        int k_i_1 = 0;
        String plaintext = "";
        while (c_i < _runeLen(ciphertext)) {
            String c_char = ciphertext.substring(c_i, c_i+1);
            int c_idx_1 = char_index(c_char);
            if (c_idx_1 < 0) {
                plaintext = plaintext + c_char;
            } else {
                String k_char_1 = current_key.substring(k_i_1, k_i_1+1);
                int k_idx_1 = char_index(k_char_1);
                int p_idx_1 = Math.floorMod((c_idx_1 - k_idx_1 + 26), 26);
                String p_char_1 = String.valueOf(index_char(p_idx_1));
                plaintext = plaintext + p_char_1;
                current_key = current_key + p_char_1;
                k_i_1 = k_i_1 + 1;
            }
            c_i = c_i + 1;
        }
        return plaintext;
    }
    public static void main(String[] args) {
        LOWER = "abcdefghijklmnopqrstuvwxyz";
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        System.out.println(encrypt("hello world", "coffee"));
        System.out.println(decrypt("jsqqs avvwo", "coffee"));
        System.out.println(encrypt("coffee is good as python", "TheAlgorithms"));
        System.out.println(decrypt("vvjfpk wj ohvp su ddylsv", "TheAlgorithms"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
