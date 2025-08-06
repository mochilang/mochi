public class Main {
    static String SYMBOLS;

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int temp = Math.floorMod(x, y);
            x = y;
            y = temp;
        }
        return x;
    }

    static int mod_inverse(int a, int m) {
        if (gcd(a, m) != 1) {
            throw new RuntimeException(String.valueOf("mod inverse of " + _p(a) + " and " + _p(m) + " does not exist"));
        }
        int u1 = 1;
        int u2 = 0;
        int u3 = a;
        int v1 = 0;
        int v2 = 1;
        int v3 = m;
        while (v3 != 0) {
            int q = u3 / v3;
            int t1 = u1 - q * v1;
            int t2 = u2 - q * v2;
            int t3 = u3 - q * v3;
            u1 = v1;
            u2 = v2;
            u3 = v3;
            v1 = t1;
            v2 = t2;
            v3 = t3;
        }
        int res = Math.floorMod(u1, m);
        if (res < 0) {
            return res + m;
        }
        return res;
    }

    static int find_symbol(String ch) {
        int i = 0;
        while (i < _runeLen(SYMBOLS)) {
            if ((SYMBOLS.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static void check_keys(int key_a, int key_b, String mode) {
        int m = _runeLen(SYMBOLS);
        if ((mode.equals("encrypt"))) {
            if (key_a == 1) {
                throw new RuntimeException(String.valueOf("The affine cipher becomes weak when key A is set to 1. Choose different key"));
            }
            if (key_b == 0) {
                throw new RuntimeException(String.valueOf("The affine cipher becomes weak when key B is set to 0. Choose different key"));
            }
        }
        if (key_a < 0 || key_b < 0 || key_b > m - 1) {
            throw new RuntimeException(String.valueOf("Key A must be greater than 0 and key B must be between 0 and " + _p(m - 1)));
        }
        if (gcd(key_a, m) != 1) {
            throw new RuntimeException(String.valueOf("Key A " + _p(key_a) + " and the symbol set size " + _p(m) + " are not relatively prime. Choose a different key."));
        }
    }

    static String encrypt_message(int key, String message) {
        int m_1 = _runeLen(SYMBOLS);
        int key_a = key / m_1;
        int key_b = Math.floorMod(key, m_1);
        check_keys(key_a, key_b, "encrypt");
        String cipher_text = "";
        int i_1 = 0;
        while (i_1 < _runeLen(message)) {
            String ch = message.substring(i_1, i_1+1);
            int index = find_symbol(ch);
            if (index >= 0) {
                cipher_text = cipher_text + SYMBOLS.substring(Math.floorMod((index * key_a + key_b), m_1), Math.floorMod((index * key_a + key_b), m_1)+1);
            } else {
                cipher_text = cipher_text + ch;
            }
            i_1 = i_1 + 1;
        }
        return cipher_text;
    }

    static String decrypt_message(int key, String message) {
        int m_2 = _runeLen(SYMBOLS);
        int key_a_1 = key / m_2;
        int key_b_1 = Math.floorMod(key, m_2);
        check_keys(key_a_1, key_b_1, "decrypt");
        int inv = mod_inverse(key_a_1, m_2);
        String plain_text = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String ch_1 = message.substring(i_2, i_2+1);
            int index_1 = find_symbol(ch_1);
            if (index_1 >= 0) {
                int n = (index_1 - key_b_1) * inv;
                int pos = Math.floorMod(n, m_2);
                int final_ = pos < 0 ? pos + m_2 : pos;
                plain_text = plain_text + SYMBOLS.substring(final_, final_+1);
            } else {
                plain_text = plain_text + ch_1;
            }
            i_2 = i_2 + 1;
        }
        return plain_text;
    }

    static void main() {
        int key = 4545;
        String msg = "The affine cipher is a type of monoalphabetic substitution cipher.";
        String enc = String.valueOf(encrypt_message(key, msg));
        System.out.println(enc);
        System.out.println(decrypt_message(key, enc));
    }
    public static void main(String[] args) {
        SYMBOLS = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        main();
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
