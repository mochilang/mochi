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

    static long pow2(long n) {
        long res = 1L;
        long i_2 = 0L;
        while (i_2 < n) {
            res = res * 2;
            i_2 = i_2 + 1;
        }
        return res;
    }

    static long bit_and(long a, long b) {
        long x = a;
        long y_1 = b;
        long res_2 = 0L;
        long bit_1 = 1L;
        long i_4 = 0L;
        while (i_4 < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y_1, 2) == 1)) {
                res_2 = res_2 + bit_1;
            }
            x = Math.floorDiv(x, 2);
            y_1 = Math.floorDiv(y_1, 2);
            bit_1 = bit_1 * 2;
            i_4 = i_4 + 1;
        }
        return res_2;
    }

    static long bit_or(long a, long b) {
        long x_1 = a;
        long y_3 = b;
        long res_4 = 0L;
        long bit_3 = 1L;
        long i_6 = 0L;
        while (i_6 < 32) {
            long abit_1 = Math.floorMod(x_1, 2);
            long bbit_1 = Math.floorMod(y_3, 2);
            if (abit_1 == 1 || bbit_1 == 1) {
                res_4 = res_4 + bit_3;
            }
            x_1 = Math.floorDiv(x_1, 2);
            y_3 = Math.floorDiv(y_3, 2);
            bit_3 = bit_3 * 2;
            i_6 = i_6 + 1;
        }
        return res_4;
    }

    static long bit_xor(long a, long b) {
        long x_2 = a;
        long y_5 = b;
        long res_6 = 0L;
        long bit_5 = 1L;
        long i_8 = 0L;
        while (i_8 < 32) {
            long abit_3 = Math.floorMod(x_2, 2);
            long bbit_3 = Math.floorMod(y_5, 2);
            if ((abit_3 == 1 && bbit_3 == 0) || (abit_3 == 0 && bbit_3 == 1)) {
                res_6 = res_6 + bit_5;
            }
            x_2 = Math.floorDiv(x_2, 2);
            y_5 = Math.floorDiv(y_5, 2);
            bit_5 = bit_5 * 2;
            i_8 = i_8 + 1;
        }
        return res_6;
    }

    static long bit_not(long a) {
        return (MOD - 1) - a;
    }

    static long rotate_left(long n, long b) {
        long left = Math.floorMod((n * pow2(b)), MOD);
        long right_1 = Math.floorDiv(n, pow2(32 - b));
        return Math.floorMod((left + right_1), MOD);
    }

    static String to_hex32(long n) {
        String digits = "0123456789abcdef";
        long num_1 = n;
        String s_1 = "";
        if (num_1 == 0) {
            s_1 = "0";
        }
        while (num_1 > 0) {
            long d_1 = Math.floorMod(num_1, 16);
            s_1 = _substr(digits, (int)((long)(d_1)), (int)((long)(d_1 + 1)))) + s_1;
            num_1 = Math.floorDiv(num_1, 16);
        }
        while (_runeLen(s_1) < 8) {
            s_1 = "0" + s_1;
        }
        if (_runeLen(s_1) > 8) {
            s_1 = _substr(s_1, (int)((long)(_runeLen(s_1) - 8)), (int)((long)(_runeLen(s_1)))));
        }
        return s_1;
    }

    static String sha1(String message) {
        long[] bytes = ((long[])(new long[]{}));
        long i_10 = 0L;
        while (i_10 < _runeLen(message)) {
            bytes = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(bytes), java.util.stream.LongStream.of(ord(_substr(message, (int)((long)(i_10)), (int)((long)(i_10 + 1))))))).toArray()));
            i_10 = i_10 + 1;
        }
        bytes = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(bytes), java.util.stream.LongStream.of(128L)).toArray()));
        while (Math.floorMod((bytes.length + 8), 64) != 0) {
            bytes = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(bytes), java.util.stream.LongStream.of(0L)).toArray()));
        }
        long bit_len_1 = _runeLen(message) * 8;
        long[] len_bytes_1 = ((long[])(new long[]{0, 0, 0, 0, 0, 0, 0, 0}));
        long bl_1 = bit_len_1;
        long k_1 = 7L;
        while (k_1 >= 0) {
len_bytes_1[(int)((long)(k_1))] = Math.floorMod(bl_1, 256);
            bl_1 = Math.floorDiv(bl_1, 256);
            k_1 = k_1 - 1;
        }
        long j_1 = 0L;
        while (j_1 < 8) {
            bytes = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(bytes), java.util.stream.LongStream.of(len_bytes_1[(int)((long)(j_1))])).toArray()));
            j_1 = j_1 + 1;
        }
        long[][] blocks_1 = ((long[][])(new long[][]{}));
        long pos_1 = 0L;
        while (pos_1 < bytes.length) {
            long[] block_1 = ((long[])(new long[]{}));
            long j2_1 = 0L;
            while (j2_1 < 64) {
                block_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(block_1), java.util.stream.LongStream.of(bytes[(int)((long)(pos_1 + j2_1))])).toArray()));
                j2_1 = j2_1 + 1;
            }
            blocks_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(blocks_1), java.util.stream.Stream.of(block_1)).toArray(long[][]::new)));
            pos_1 = pos_1 + 64;
        }
        long h0_1 = 1732584193L;
        long h1_1 = 4023233417L;
        long h2_1 = 2562383102L;
        long h3_1 = 271733878L;
        long h4_1 = 3285377520L;
        long bindex_1 = 0L;
        while (bindex_1 < blocks_1.length) {
            long[] block_3 = ((long[])(blocks_1[(int)((long)(bindex_1))]));
            long[] w_1 = ((long[])(new long[]{}));
            long t_1 = 0L;
            while (t_1 < 16) {
                long j3_1 = t_1 * 4;
                long word_1 = (((block_3[(int)((long)(j3_1))] * 256 + block_3[(int)((long)(j3_1 + 1))]) * 256 + block_3[(int)((long)(j3_1 + 2))]) * 256 + block_3[(int)((long)(j3_1 + 3))]);
                w_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(w_1), java.util.stream.LongStream.of(word_1)).toArray()));
                t_1 = t_1 + 1;
            }
            while (t_1 < 80) {
                long tmp_1 = bit_xor(bit_xor(bit_xor(w_1[(int)((long)(t_1 - 3))], w_1[(int)((long)(t_1 - 8))]), w_1[(int)((long)(t_1 - 14))]), w_1[(int)((long)(t_1 - 16))]);
                w_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(w_1), java.util.stream.LongStream.of(rotate_left(tmp_1, 1L))).toArray()));
                t_1 = t_1 + 1;
            }
            long a_1 = h0_1;
            long b_1 = h1_1;
            long c_1 = h2_1;
            long d_3 = h3_1;
            long e_1 = h4_1;
            long i2_1 = 0L;
            while (i2_1 < 80) {
                long f_1 = 0L;
                long kconst_1 = 0L;
                if (i2_1 < 20) {
                    f_1 = bit_or(bit_and(b_1, c_1), bit_and(bit_not(b_1), d_3));
                    kconst_1 = 1518500249;
                } else                 if (i2_1 < 40) {
                    f_1 = bit_xor(bit_xor(b_1, c_1), d_3);
                    kconst_1 = 1859775393;
                } else                 if (i2_1 < 60) {
                    f_1 = bit_or(bit_or(bit_and(b_1, c_1), bit_and(b_1, d_3)), bit_and(c_1, d_3));
                    kconst_1 = 2400959708L;
                } else {
                    f_1 = bit_xor(bit_xor(b_1, c_1), d_3);
                    kconst_1 = 3395469782L;
                }
                long temp_1 = Math.floorMod((rotate_left(a_1, 5L) + f_1 + e_1 + kconst_1 + w_1[(int)((long)(i2_1))]), MOD);
                e_1 = d_3;
                d_3 = c_1;
                c_1 = rotate_left(b_1, 30L);
                b_1 = a_1;
                a_1 = temp_1;
                i2_1 = i2_1 + 1;
            }
            h0_1 = Math.floorMod((h0_1 + a_1), MOD);
            h1_1 = Math.floorMod((h1_1 + b_1), MOD);
            h2_1 = Math.floorMod((h2_1 + c_1), MOD);
            h3_1 = Math.floorMod((h3_1 + d_3), MOD);
            h4_1 = Math.floorMod((h4_1 + e_1), MOD);
            bindex_1 = bindex_1 + 1;
        }
        return ((String)(String.valueOf(String.valueOf(String.valueOf(String.valueOf(to_hex32(h0_1)) + String.valueOf(to_hex32(h1_1))) + String.valueOf(to_hex32(h2_1))) + String.valueOf(to_hex32(h3_1))) + String.valueOf(to_hex32(h4_1))));
    }

    static void main() {
        System.out.println(sha1("Test String"));
    }
    public static void main(String[] args) {
        MOD = 4294967296L;
        ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
        main();
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
}
