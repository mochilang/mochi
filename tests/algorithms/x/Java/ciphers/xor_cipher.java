public class Main {
    static String ascii;
    static String sample;
    static String enc;
    static String dec;

    static int xor(int a, int b) {
        int res = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int abit = Math.floorMod(x, 2);
            int bbit = Math.floorMod(y, 2);
            if (abit != bbit) {
                res = res + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
        }
        return res;
    }

    static int ord(String ch) {
        int i = 0;
        while (i < _runeLen(ascii)) {
            if ((ascii.substring(i, i + 1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static String chr(int n) {
        if (n >= 32 && n < 127) {
            return ascii.substring(n - 32, n - 31);
        }
        return "";
    }

    static int normalize_key(int key) {
        int k = key;
        if (k == 0) {
            k = 1;
        }
        k = Math.floorMod(k, 256);
        if (k < 0) {
            k = k + 256;
        }
        return k;
    }

    static String[] encrypt(String content, int key) {
        int k_1 = normalize_key(key);
        String[] result = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(content)) {
            int c = ord(content.substring(i_1, i_1 + 1));
            int e = xor(c, k_1);
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(chr(e))).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String encrypt_string(String content, int key) {
        String[] chars = ((String[])(encrypt(content, key)));
        String out = "";
        for (String ch : chars) {
            out = out + ch;
        }
        return out;
    }
    public static void main(String[] args) {
        ascii = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        sample = "hallo welt";
        enc = String.valueOf(encrypt_string(sample, 1));
        dec = String.valueOf(encrypt_string(enc, 1));
        System.out.println(_p(encrypt(sample, 1)));
        System.out.println(enc);
        System.out.println(dec);
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
