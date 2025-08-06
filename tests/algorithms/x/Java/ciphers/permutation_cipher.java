public class Main {
    static int seed = 0;
    static String message;
    static int block_size;
    static int[] key;
    static String encrypted_1;
    static String decrypted_1;

    static int rand(int max) {
        seed = Math.floorMod((seed * 1103515245 + 12345), 2147483647);
        return Math.floorMod(seed, max);
    }

    static int generate_valid_block_size(int message_length) {
        int[] factors = ((int[])(new int[]{}));
        int i = 2;
        while (i <= message_length) {
            if (Math.floorMod(message_length, i) == 0) {
                factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        int idx = rand(factors.length);
        return factors[idx];
    }

    static int[] generate_permutation_key(int block_size) {
        int[] digits = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < block_size) {
            digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        int j = block_size - 1;
        while (j > 0) {
            int k = rand(j + 1);
            int temp = digits[j];
digits[j] = digits[k];
digits[k] = temp;
            j = j - 1;
        }
        return digits;
    }

    static String encrypt(String message, int[] key, int block_size) {
        String encrypted = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String block = _substr(message, i_2, i_2 + block_size);
            int j_1 = 0;
            while (j_1 < block_size) {
                encrypted = encrypted + _substr(block, key[j_1], key[j_1] + 1);
                j_1 = j_1 + 1;
            }
            i_2 = i_2 + block_size;
        }
        return encrypted;
    }

    static String[] repeat_string(int times) {
        String[] res = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < times) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of("")).toArray(String[]::new)));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static String decrypt(String encrypted, int[] key) {
        int klen = key.length;
        String decrypted = "";
        int i_4 = 0;
        while (i_4 < _runeLen(encrypted)) {
            String block_1 = _substr(encrypted, i_4, i_4 + klen);
            String[] original = ((String[])(repeat_string(klen)));
            int j_2 = 0;
            while (j_2 < klen) {
original[key[j_2]] = _substr(block_1, j_2, j_2 + 1);
                j_2 = j_2 + 1;
            }
            j_2 = 0;
            while (j_2 < klen) {
                decrypted = decrypted + original[j_2];
                j_2 = j_2 + 1;
            }
            i_4 = i_4 + klen;
        }
        return decrypted;
    }
    public static void main(String[] args) {
        seed = 1;
        message = "HELLO WORLD";
        block_size = generate_valid_block_size(_runeLen(message));
        key = ((int[])(generate_permutation_key(block_size)));
        encrypted_1 = String.valueOf(encrypt(message, ((int[])(key)), block_size));
        decrypted_1 = String.valueOf(decrypt(encrypted_1, ((int[])(key))));
        System.out.println("Block size: " + _p(block_size));
        System.out.println("Key: " + _p(key));
        System.out.println("Encrypted: " + encrypted_1);
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
