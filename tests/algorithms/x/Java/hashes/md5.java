public class Main {
    static int MOD;
    static String ASCII;

    static int ord(String ch) {
        int i = 0;
        while (i < _runeLen(ASCII)) {
            if ((ASCII.substring(i, i + 1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static String to_little_endian(String s) {
        if (_runeLen(s) != 32) {
            throw new RuntimeException(String.valueOf("Input must be of length 32"));
        }
        return s.substring(24, 32) + s.substring(16, 24) + s.substring(8, 16) + s.substring(0, 8);
    }

    static String int_to_bits(int n, int width) {
        String bits = "";
        int num = n;
        while (num > 0) {
            bits = _p(Math.floorMod(num, 2)) + bits;
            num = num / 2;
        }
        while (_runeLen(bits) < width) {
            bits = "0" + bits;
        }
        if (_runeLen(bits) > width) {
            bits = bits.substring(_runeLen(bits) - width, _runeLen(bits));
        }
        return bits;
    }

    static int bits_to_int(String bits) {
        int num_1 = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(bits)) {
            if ((bits.substring(i_1, i_1 + 1).equals("1"))) {
                num_1 = num_1 * 2 + 1;
            } else {
                num_1 = num_1 * 2;
            }
            i_1 = i_1 + 1;
        }
        return num_1;
    }

    static String to_hex(int n) {
        String digits = "0123456789abcdef";
        if (n == 0) {
            return "0";
        }
        int num_2 = n;
        String s = "";
        while (num_2 > 0) {
            int d = Math.floorMod(num_2, 16);
            s = digits.substring(d, d + 1) + s;
            num_2 = num_2 / 16;
        }
        return s;
    }

    static String reformat_hex(int i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        String hex = String.valueOf(to_hex(i));
        while (_runeLen(hex) < 8) {
            hex = "0" + hex;
        }
        if (_runeLen(hex) > 8) {
            hex = hex.substring(_runeLen(hex) - 8, _runeLen(hex));
        }
        String le = "";
        int j = _runeLen(hex) - 2;
        while (j >= 0) {
            le = le + hex.substring(j, j + 2);
            j = j - 2;
        }
        return le;
    }

    static String preprocess(String message) {
        String bit_string = "";
        int i_2 = 0;
        while (i_2 < _runeLen(message)) {
            String ch = message.substring(i_2, i_2 + 1);
            bit_string = bit_string + String.valueOf(int_to_bits(ord(ch), 8));
            i_2 = i_2 + 1;
        }
        String start_len = String.valueOf(int_to_bits(_runeLen(bit_string), 64));
        bit_string = bit_string + "1";
        while (Math.floorMod(_runeLen(bit_string), 512) != 448) {
            bit_string = bit_string + "0";
        }
        bit_string = bit_string + String.valueOf(to_little_endian(start_len.substring(32, 64))) + String.valueOf(to_little_endian(start_len.substring(0, 32)));
        return bit_string;
    }

    static int[][] get_block_words(String bit_string) {
        if (Math.floorMod(_runeLen(bit_string), 512) != 0) {
            throw new RuntimeException(String.valueOf("Input must have length that's a multiple of 512"));
        }
        int[][] blocks = ((int[][])(new int[][]{}));
        int pos = 0;
        while (pos < _runeLen(bit_string)) {
            int[] block = ((int[])(new int[]{}));
            int i_3 = 0;
            while (i_3 < 512) {
                String part = bit_string.substring(pos + i_3, pos + i_3 + 32);
                int word = bits_to_int(String.valueOf(to_little_endian(part)));
                block = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(block), java.util.stream.IntStream.of(word)).toArray()));
                i_3 = i_3 + 32;
            }
            blocks = ((int[][])(appendObj(blocks, block)));
            pos = pos + 512;
        }
        return blocks;
    }

    static int bit_and(int a, int b) {
        int x = a;
        int y = b;
        int res = 0;
        int bit = 1;
        int i_4 = 0;
        while (i_4 < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y, 2) == 1)) {
                res = res + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
            i_4 = i_4 + 1;
        }
        return res;
    }

    static int bit_or(int a, int b) {
        int x_1 = a;
        int y_1 = b;
        int res_1 = 0;
        int bit_1 = 1;
        int i_5 = 0;
        while (i_5 < 32) {
            int abit = Math.floorMod(x_1, 2);
            int bbit = Math.floorMod(y_1, 2);
            if (abit == 1 || bbit == 1) {
                res_1 = res_1 + bit_1;
            }
            x_1 = x_1 / 2;
            y_1 = y_1 / 2;
            bit_1 = bit_1 * 2;
            i_5 = i_5 + 1;
        }
        return res_1;
    }

    static int bit_xor(int a, int b) {
        int x_2 = a;
        int y_2 = b;
        int res_2 = 0;
        int bit_2 = 1;
        int i_6 = 0;
        while (i_6 < 32) {
            int abit_1 = Math.floorMod(x_2, 2);
            int bbit_1 = Math.floorMod(y_2, 2);
            if (Math.floorMod((abit_1 + bbit_1), 2) == 1) {
                res_2 = res_2 + bit_2;
            }
            x_2 = x_2 / 2;
            y_2 = y_2 / 2;
            bit_2 = bit_2 * 2;
            i_6 = i_6 + 1;
        }
        return res_2;
    }

    static int not_32(int i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        return (int)4294967295L - i;
    }

    static int sum_32(int a, int b) {
        return Math.floorMod((a + b), MOD);
    }

    static int lshift(int num, int k) {
        int result = Math.floorMod(num, MOD);
        int i_7 = 0;
        while (i_7 < k) {
            result = Math.floorMod((result * 2), MOD);
            i_7 = i_7 + 1;
        }
        return result;
    }

    static int rshift(int num, int k) {
        int result_1 = num;
        int i_8 = 0;
        while (i_8 < k) {
            result_1 = result_1 / 2;
            i_8 = i_8 + 1;
        }
        return result_1;
    }

    static int left_rotate_32(int i, int shift) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        if (shift < 0) {
            throw new RuntimeException(String.valueOf("Shift must be non-negative"));
        }
        int left = lshift(i, shift);
        int right = rshift(i, 32 - shift);
        return Math.floorMod((left + right), MOD);
    }

    static String md5_me(String message) {
        String bit_string_1 = String.valueOf(preprocess(message));
        int[] added_consts = ((int[])(new int[]{(int)3614090360L, (int)3905402710L, 606105819, (int)3250441966L, (int)4118548399L, 1200080426, (int)2821735955L, (int)4249261313L, 1770035416, (int)2336552879L, (int)4294925233L, (int)2304563134L, 1804603682, (int)4254626195L, (int)2792965006L, 1236535329, (int)4129170786L, (int)3225465664L, 643717713, (int)3921069994L, (int)3593408605L, 38016083, (int)3634488961L, (int)3889429448L, 568446438, (int)3275163606L, (int)4107603335L, 1163531501, (int)2850285829L, (int)4243563512L, 1735328473, (int)2368359562L, (int)4294588738L, (int)2272392833L, 1839030562, (int)4259657740L, (int)2763975236L, 1272893353, (int)4139469664L, (int)3200236656L, 681279174, (int)3936430074L, (int)3572445317L, 76029189, (int)3654602809L, (int)3873151461L, 530742520, (int)3299628645L, (int)4096336452L, 1126891415, (int)2878612391L, (int)4237533241L, 1700485571, (int)2399980690L, (int)4293915773L, (int)2240044497L, 1873313359, (int)4264355552L, (int)2734768916L, 1309151649, (int)4149444226L, (int)3174756917L, 718787259, (int)3951481745L}));
        int[] shift_amounts = ((int[])(new int[]{7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21}));
        int a0 = 1732584193;
        int b0 = (int)4023233417L;
        int c0 = (int)2562383102L;
        int d0 = 271733878;
        int[][] blocks_1 = ((int[][])(get_block_words(bit_string_1)));
        int bi = 0;
        while (bi < blocks_1.length) {
            int[] block_1 = ((int[])(blocks_1[bi]));
            int a = a0;
            int b = b0;
            int c = c0;
            int d_1 = d0;
            int i_9 = 0;
            while (i_9 < 64) {
                int f = 0;
                int g = 0;
                if (i_9 <= 15) {
                    f = bit_xor(d_1, bit_and(b, bit_xor(c, d_1)));
                    g = i_9;
                } else                 if (i_9 <= 31) {
                    f = bit_xor(c, bit_and(d_1, bit_xor(b, c)));
                    g = Math.floorMod((5 * i_9 + 1), 16);
                } else                 if (i_9 <= 47) {
                    f = bit_xor(bit_xor(b, c), d_1);
                    g = Math.floorMod((3 * i_9 + 5), 16);
                } else {
                    f = bit_xor(c, bit_or(b, not_32(d_1)));
                    g = Math.floorMod((7 * i_9), 16);
                }
                f = sum_32(f, a);
                f = sum_32(f, added_consts[i_9]);
                f = sum_32(f, block_1[g]);
                int rotated = left_rotate_32(f, shift_amounts[i_9]);
                int new_b = sum_32(b, rotated);
                a = d_1;
                d_1 = c;
                c = b;
                b = new_b;
                i_9 = i_9 + 1;
            }
            a0 = sum_32(a0, a);
            b0 = sum_32(b0, b);
            c0 = sum_32(c0, c);
            d0 = sum_32(d0, d_1);
            bi = bi + 1;
        }
        String digest = String.valueOf(String.valueOf(String.valueOf(String.valueOf(reformat_hex(a0)) + String.valueOf(reformat_hex(b0))) + String.valueOf(reformat_hex(c0))) + String.valueOf(reformat_hex(d0)));
        return digest;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            MOD = (int)4294967296L;
            ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
