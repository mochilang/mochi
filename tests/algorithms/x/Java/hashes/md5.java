public class Main {
    static long MOD;
    static String ASCII;

    static long ord(String ch) {
        long i = 0L;
        while (i < _runeLen(ASCII)) {
            if ((_substr(ASCII, (int)((long)(i)), (int)((long)(i + 1)))).equals(ch))) {
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
        return _substr(s, (int)((long)(24)), (int)((long)(32)))) + _substr(s, (int)((long)(16)), (int)((long)(24)))) + _substr(s, (int)((long)(8)), (int)((long)(16)))) + _substr(s, (int)((long)(0)), (int)((long)(8))));
    }

    static String int_to_bits(long n, long width) {
        String bits = "";
        long num_1 = n;
        while (num_1 > 0) {
            bits = _p(Math.floorMod(num_1, 2)) + bits;
            num_1 = Math.floorDiv(num_1, 2);
        }
        while (_runeLen(bits) < width) {
            bits = "0" + bits;
        }
        if (_runeLen(bits) > width) {
            bits = _substr(bits, (int)((long)(_runeLen(bits) - width)), (int)((long)(_runeLen(bits)))));
        }
        return bits;
    }

    static long bits_to_int(String bits) {
        long num_2 = 0L;
        long i_2 = 0L;
        while (i_2 < _runeLen(bits)) {
            if ((_substr(bits, (int)((long)(i_2)), (int)((long)(i_2 + 1)))).equals("1"))) {
                num_2 = num_2 * 2 + 1;
            } else {
                num_2 = num_2 * 2;
            }
            i_2 = i_2 + 1;
        }
        return num_2;
    }

    static String to_hex(long n) {
        String digits = "0123456789abcdef";
        if (n == 0) {
            return "0";
        }
        long num_4 = n;
        String s_1 = "";
        while (num_4 > 0) {
            long d_1 = Math.floorMod(num_4, 16);
            s_1 = _substr(digits, (int)((long)(d_1)), (int)((long)(d_1 + 1)))) + s_1;
            num_4 = Math.floorDiv(num_4, 16);
        }
        return s_1;
    }

    static String reformat_hex(long i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        String hex_1 = String.valueOf(to_hex(i));
        while (_runeLen(hex_1) < 8) {
            hex_1 = "0" + hex_1;
        }
        if (_runeLen(hex_1) > 8) {
            hex_1 = _substr(hex_1, (int)((long)(_runeLen(hex_1) - 8)), (int)((long)(_runeLen(hex_1)))));
        }
        String le_1 = "";
        long j_1 = _runeLen(hex_1) - 2;
        while (j_1 >= 0) {
            le_1 = le_1 + _substr(hex_1, (int)((long)(j_1)), (int)((long)(j_1 + 2))));
            j_1 = j_1 - 2;
        }
        return le_1;
    }

    static String preprocess(String message) {
        String bit_string = "";
        long i_4 = 0L;
        while (i_4 < _runeLen(message)) {
            String ch_1 = _substr(message, (int)((long)(i_4)), (int)((long)(i_4 + 1))));
            bit_string = bit_string + String.valueOf(int_to_bits(ord(ch_1), 8L));
            i_4 = i_4 + 1;
        }
        String start_len_1 = String.valueOf(int_to_bits(_runeLen(bit_string), 64L));
        bit_string = bit_string + "1";
        while (Math.floorMod(_runeLen(bit_string), 512) != 448) {
            bit_string = bit_string + "0";
        }
        bit_string = bit_string + String.valueOf(to_little_endian(_substr(start_len_1, (int)((long)(32)), (int)((long)(64)))))) + String.valueOf(to_little_endian(_substr(start_len_1, (int)((long)(0)), (int)((long)(32))))));
        return bit_string;
    }

    static long[][] get_block_words(String bit_string) {
        if (Math.floorMod(_runeLen(bit_string), 512) != 0) {
            throw new RuntimeException(String.valueOf("Input must have length that's a multiple of 512"));
        }
        long[][] blocks_1 = ((long[][])(new long[][]{}));
        long pos_1 = 0L;
        while (pos_1 < _runeLen(bit_string)) {
            long[] block_1 = ((long[])(new long[]{}));
            long i_6 = 0L;
            while (i_6 < 512) {
                String part_1 = _substr(bit_string, (int)((long)(pos_1 + i_6)), (int)((long)(pos_1 + i_6 + 32))));
                long word_1 = bits_to_int(String.valueOf(to_little_endian(part_1)));
                block_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(block_1), java.util.stream.LongStream.of(word_1)).toArray()));
                i_6 = i_6 + 32;
            }
            blocks_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(blocks_1), java.util.stream.Stream.of(block_1)).toArray(long[][]::new)));
            pos_1 = pos_1 + 512;
        }
        return blocks_1;
    }

    static long bit_and(long a, long b) {
        long x = a;
        long y_1 = b;
        long res_1 = 0L;
        long bit_1 = 1L;
        long i_8 = 0L;
        while (i_8 < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y_1, 2) == 1)) {
                res_1 = res_1 + bit_1;
            }
            x = Math.floorDiv(x, 2);
            y_1 = Math.floorDiv(y_1, 2);
            bit_1 = bit_1 * 2;
            i_8 = i_8 + 1;
        }
        return res_1;
    }

    static long bit_or(long a, long b) {
        long x_1 = a;
        long y_3 = b;
        long res_3 = 0L;
        long bit_3 = 1L;
        long i_10 = 0L;
        while (i_10 < 32) {
            long abit_1 = Math.floorMod(x_1, 2);
            long bbit_1 = Math.floorMod(y_3, 2);
            if (abit_1 == 1 || bbit_1 == 1) {
                res_3 = res_3 + bit_3;
            }
            x_1 = Math.floorDiv(x_1, 2);
            y_3 = Math.floorDiv(y_3, 2);
            bit_3 = bit_3 * 2;
            i_10 = i_10 + 1;
        }
        return res_3;
    }

    static long bit_xor(long a, long b) {
        long x_2 = a;
        long y_5 = b;
        long res_5 = 0L;
        long bit_5 = 1L;
        long i_12 = 0L;
        while (i_12 < 32) {
            long abit_3 = Math.floorMod(x_2, 2);
            long bbit_3 = Math.floorMod(y_5, 2);
            if (Math.floorMod((abit_3 + bbit_3), 2) == 1) {
                res_5 = res_5 + bit_5;
            }
            x_2 = Math.floorDiv(x_2, 2);
            y_5 = Math.floorDiv(y_5, 2);
            bit_5 = bit_5 * 2;
            i_12 = i_12 + 1;
        }
        return res_5;
    }

    static long not_32(long i) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        return 4294967295L - i;
    }

    static long sum_32(long a, long b) {
        return Math.floorMod((a + b), MOD);
    }

    static long lshift(long num, long k) {
        long result = Math.floorMod(num, MOD);
        long i_14 = 0L;
        while (i_14 < k) {
            result = Math.floorMod((result * 2), MOD);
            i_14 = i_14 + 1;
        }
        return result;
    }

    static long rshift(long num, long k) {
        long result_1 = num;
        long i_16 = 0L;
        while (i_16 < k) {
            result_1 = Math.floorDiv(result_1, 2);
            i_16 = i_16 + 1;
        }
        return result_1;
    }

    static long left_rotate_32(long i, long shift) {
        if (i < 0) {
            throw new RuntimeException(String.valueOf("Input must be non-negative"));
        }
        if (shift < 0) {
            throw new RuntimeException(String.valueOf("Shift must be non-negative"));
        }
        long left_1 = lshift(i, shift);
        long right_1 = rshift(i, 32 - shift);
        return Math.floorMod((left_1 + right_1), MOD);
    }

    static String md5_me(String message) {
        String bit_string_1 = String.valueOf(preprocess(message));
        long[] added_consts_1 = ((long[])(new long[]{3614090360L, 3905402710L, 606105819, 3250441966L, 4118548399L, 1200080426, 2821735955L, 4249261313L, 1770035416, 2336552879L, 4294925233L, 2304563134L, 1804603682, 4254626195L, 2792965006L, 1236535329, 4129170786L, 3225465664L, 643717713, 3921069994L, 3593408605L, 38016083, 3634488961L, 3889429448L, 568446438, 3275163606L, 4107603335L, 1163531501, 2850285829L, 4243563512L, 1735328473, 2368359562L, 4294588738L, 2272392833L, 1839030562, 4259657740L, 2763975236L, 1272893353, 4139469664L, 3200236656L, 681279174, 3936430074L, 3572445317L, 76029189, 3654602809L, 3873151461L, 530742520, 3299628645L, 4096336452L, 1126891415, 2878612391L, 4237533241L, 1700485571, 2399980690L, 4293915773L, 2240044497L, 1873313359, 4264355552L, 2734768916L, 1309151649, 4149444226L, 3174756917L, 718787259, 3951481745L}));
        long[] shift_amounts_1 = ((long[])(new long[]{7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21}));
        long a0_1 = 1732584193L;
        long b0_1 = 4023233417L;
        long c0_1 = 2562383102L;
        long d0_1 = 271733878L;
        long[][] blocks_3 = ((long[][])(get_block_words(bit_string_1)));
        long bi_1 = 0L;
        while (bi_1 < blocks_3.length) {
            long[] block_3 = ((long[])(blocks_3[(int)((long)(bi_1))]));
            long a_1 = a0_1;
            long b_1 = b0_1;
            long c_1 = c0_1;
            long d_3 = d0_1;
            long i_18 = 0L;
            while (i_18 < 64) {
                long f_1 = 0L;
                long g_1 = 0L;
                if (i_18 <= 15) {
                    f_1 = bit_xor(d_3, bit_and(b_1, bit_xor(c_1, d_3)));
                    g_1 = i_18;
                } else                 if (i_18 <= 31) {
                    f_1 = bit_xor(c_1, bit_and(d_3, bit_xor(b_1, c_1)));
                    g_1 = Math.floorMod((5 * i_18 + 1), 16);
                } else                 if (i_18 <= 47) {
                    f_1 = bit_xor(bit_xor(b_1, c_1), d_3);
                    g_1 = Math.floorMod((3 * i_18 + 5), 16);
                } else {
                    f_1 = bit_xor(c_1, bit_or(b_1, not_32(d_3)));
                    g_1 = Math.floorMod((7 * i_18), 16);
                }
                f_1 = sum_32(f_1, a_1);
                f_1 = sum_32(f_1, added_consts_1[(int)((long)(i_18))]);
                f_1 = sum_32(f_1, block_3[(int)((long)(g_1))]);
                long rotated_1 = left_rotate_32(f_1, shift_amounts_1[(int)((long)(i_18))]);
                long new_b_1 = sum_32(b_1, rotated_1);
                a_1 = d_3;
                d_3 = c_1;
                c_1 = b_1;
                b_1 = new_b_1;
                i_18 = i_18 + 1;
            }
            a0_1 = sum_32(a0_1, a_1);
            b0_1 = sum_32(b0_1, b_1);
            c0_1 = sum_32(c0_1, c_1);
            d0_1 = sum_32(d0_1, d_3);
            bi_1 = bi_1 + 1;
        }
        String digest_1 = String.valueOf(String.valueOf(String.valueOf(String.valueOf(reformat_hex(a0_1)) + String.valueOf(reformat_hex(b0_1))) + String.valueOf(reformat_hex(c0_1))) + String.valueOf(reformat_hex(d0_1)));
        return digest_1;
    }
    public static void main(String[] args) {
        MOD = 4294967296L;
        ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
