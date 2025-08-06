public class Main {
    static int seed = 0;
    static String ascii_chars;
    static java.util.Map<String,int[]> res_1;
    static int[] cipher_1;
    static int[] key_1;

    static void set_seed(int s) {
        seed = s;
    }

    static int randint(int a, int b) {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return (Math.floorMod(seed, (b - a + 1))) + a;
    }

    static int ord(String ch) {
        int i = 0;
        while (i < _runeLen(ascii_chars)) {
            if ((ascii_chars.substring(i, i+1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static String chr(int code) {
        if (code < 32 || code > 126) {
            return "";
        }
        return ascii_chars.substring(code - 32, code - 32+1);
    }

    static java.util.Map<String,int[]> encrypt(String text) {
        int[] cipher = ((int[])(new int[]{}));
        int[] key = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(text)) {
            int p = ord(text.substring(i_1, i_1+1));
            int k = randint(1, 300);
            int c = (p + k) * k;
            cipher = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cipher), java.util.stream.IntStream.of(c)).toArray()));
            key = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(key), java.util.stream.IntStream.of(k)).toArray()));
            i_1 = i_1 + 1;
        }
        java.util.Map<String,int[]> res = ((java.util.Map<String,int[]>)(new java.util.LinkedHashMap<String, int[]>()));
res.put("cipher", ((int[])(cipher)));
res.put("key", ((int[])(key)));
        return res;
    }

    static String decrypt(int[] cipher, int[] key) {
        String plain = "";
        int i_2 = 0;
        while (i_2 < key.length) {
            int p_1 = ((cipher[i_2] - key[i_2] * key[i_2]) / key[i_2]);
            plain = plain + String.valueOf(chr(p_1));
            i_2 = i_2 + 1;
        }
        return plain;
    }
    public static void main(String[] args) {
        seed = 1;
        ascii_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        set_seed(1);
        res_1 = encrypt("Hello");
        cipher_1 = (int[])(((int[])(res_1).get("cipher")));
        key_1 = (int[])(((int[])(res_1).get("key")));
        System.out.println(java.util.Arrays.toString(cipher_1));
        System.out.println(java.util.Arrays.toString(key_1));
        System.out.println(decrypt(((int[])(cipher_1)), ((int[])(key_1))));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
