public class Main {
    static int BYTE_SIZE;

    static int pow_int(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int mod_pow(int base, int exponent, int modulus) {
        int result_1 = 1;
        int b = Math.floorMod(base, modulus);
        int e = exponent;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result_1 = Math.floorMod((result_1 * b), modulus);
            }
            e = e / 2;
            b = Math.floorMod((b * b), modulus);
        }
        return result_1;
    }

    static int ord(String ch) {
        if ((ch.equals(" "))) {
            return 32;
        }
        if ((ch.equals("a"))) {
            return 97;
        }
        if ((ch.equals("b"))) {
            return 98;
        }
        if ((ch.equals("c"))) {
            return 99;
        }
        if ((ch.equals("d"))) {
            return 100;
        }
        if ((ch.equals("e"))) {
            return 101;
        }
        if ((ch.equals("f"))) {
            return 102;
        }
        if ((ch.equals("g"))) {
            return 103;
        }
        if ((ch.equals("h"))) {
            return 104;
        }
        if ((ch.equals("i"))) {
            return 105;
        }
        if ((ch.equals("j"))) {
            return 106;
        }
        if ((ch.equals("k"))) {
            return 107;
        }
        if ((ch.equals("l"))) {
            return 108;
        }
        if ((ch.equals("m"))) {
            return 109;
        }
        if ((ch.equals("n"))) {
            return 110;
        }
        if ((ch.equals("o"))) {
            return 111;
        }
        if ((ch.equals("p"))) {
            return 112;
        }
        if ((ch.equals("q"))) {
            return 113;
        }
        if ((ch.equals("r"))) {
            return 114;
        }
        if ((ch.equals("s"))) {
            return 115;
        }
        if ((ch.equals("t"))) {
            return 116;
        }
        if ((ch.equals("u"))) {
            return 117;
        }
        if ((ch.equals("v"))) {
            return 118;
        }
        if ((ch.equals("w"))) {
            return 119;
        }
        if ((ch.equals("x"))) {
            return 120;
        }
        if ((ch.equals("y"))) {
            return 121;
        }
        if ((ch.equals("z"))) {
            return 122;
        }
        return 0;
    }

    static String chr(int code) {
        if (code == 32) {
            return " ";
        }
        if (code == 97) {
            return "a";
        }
        if (code == 98) {
            return "b";
        }
        if (code == 99) {
            return "c";
        }
        if (code == 100) {
            return "d";
        }
        if (code == 101) {
            return "e";
        }
        if (code == 102) {
            return "f";
        }
        if (code == 103) {
            return "g";
        }
        if (code == 104) {
            return "h";
        }
        if (code == 105) {
            return "i";
        }
        if (code == 106) {
            return "j";
        }
        if (code == 107) {
            return "k";
        }
        if (code == 108) {
            return "l";
        }
        if (code == 109) {
            return "m";
        }
        if (code == 110) {
            return "n";
        }
        if (code == 111) {
            return "o";
        }
        if (code == 112) {
            return "p";
        }
        if (code == 113) {
            return "q";
        }
        if (code == 114) {
            return "r";
        }
        if (code == 115) {
            return "s";
        }
        if (code == 116) {
            return "t";
        }
        if (code == 117) {
            return "u";
        }
        if (code == 118) {
            return "v";
        }
        if (code == 119) {
            return "w";
        }
        if (code == 120) {
            return "x";
        }
        if (code == 121) {
            return "y";
        }
        if (code == 122) {
            return "z";
        }
        return "";
    }

    static int[] get_blocks_from_text(String message, int block_size) {
        int[] block_ints = ((int[])(new int[]{}));
        int block_start = 0;
        while (block_start < _runeLen(message)) {
            int block_int = 0;
            int i_1 = block_start;
            while (i_1 < block_start + block_size && i_1 < _runeLen(message)) {
                block_int = block_int + ord(message.substring(i_1, i_1+1)) * pow_int(BYTE_SIZE, i_1 - block_start);
                i_1 = i_1 + 1;
            }
            block_ints = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(block_ints), java.util.stream.IntStream.of(block_int)).toArray()));
            block_start = block_start + block_size;
        }
        return block_ints;
    }

    static String get_text_from_blocks(int[] block_ints, int message_length, int block_size) {
        String message = "";
        for (int block_int : block_ints) {
            int block = block_int;
            int i_2 = block_size - 1;
            String block_message = "";
            while (i_2 >= 0) {
                if (_runeLen(message) + i_2 < message_length) {
                    int ascii_number = block / pow_int(BYTE_SIZE, i_2);
                    block = Math.floorMod(block, pow_int(BYTE_SIZE, i_2));
                    block_message = String.valueOf(chr(ascii_number)) + block_message;
                }
                i_2 = i_2 - 1;
            }
            message = message + block_message;
        }
        return message;
    }

    static int[] encrypt_message(String message, int n, int e, int block_size) {
        int[] encrypted = ((int[])(new int[]{}));
        int[] blocks = ((int[])(get_blocks_from_text(message, block_size)));
        for (int block : blocks) {
            encrypted = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(encrypted), java.util.stream.IntStream.of(mod_pow(block, e, n))).toArray()));
        }
        return encrypted;
    }

    static String decrypt_message(int[] blocks, int message_length, int n, int d, int block_size) {
        int[] decrypted_blocks = ((int[])(new int[]{}));
        for (int block : blocks) {
            decrypted_blocks = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(decrypted_blocks), java.util.stream.IntStream.of(mod_pow(block, d, n))).toArray()));
        }
        String message_1 = "";
        for (int num : decrypted_blocks) {
            message_1 = message_1 + String.valueOf(chr(num));
        }
        return message_1;
    }

    static void main() {
        String message_2 = "hello world";
        int n = 3233;
        int e_1 = 17;
        int d = 2753;
        int block_size = 1;
        int[] encrypted_1 = ((int[])(encrypt_message(message_2, n, e_1, block_size)));
        System.out.println(_p(encrypted_1));
        String decrypted = String.valueOf(decrypt_message(((int[])(encrypted_1)), _runeLen(message_2), n, d, block_size));
        System.out.println(decrypted);
    }
    public static void main(String[] args) {
        BYTE_SIZE = 256;
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
