public class Main {
    static class DecodeResult {
        long[] data;
        boolean ack;
        DecodeResult(long[] data, boolean ack) {
            this.data = data;
            this.ack = ack;
        }
        DecodeResult() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'ack': %s}", String.valueOf(data), String.valueOf(ack));
        }
    }


    static long index_of(String s, String ch) {
        long i = 0L;
        while (i < _runeLen(s)) {
            if ((s.substring((int)((long)(i)), (int)((long)(i))+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static long ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        long idx_1 = index_of(upper, ch);
        if (idx_1 >= 0) {
            return 65 + idx_1;
        }
        idx_1 = index_of(lower_1, ch);
        if (idx_1 >= 0) {
            return 97 + idx_1;
        }
        return 0;
    }

    static String chr(long n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_3 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return _substr(upper_1, (int)((long)(n - 65)), (int)((long)(n - 64))));
        }
        if (n >= 97 && n < 123) {
            return _substr(lower_3, (int)((long)(n - 97)), (int)((long)(n - 96))));
        }
        return "?";
    }

    static String text_to_bits(String text) {
        String bits = "";
        long i_2 = 0L;
        while (i_2 < _runeLen(text)) {
            long code_1 = ord(text.substring((int)((long)(i_2)), (int)((long)(i_2))+1));
            long j_1 = 7L;
            while (j_1 >= 0) {
                long p_1 = pow2(j_1);
                if ((Math.floorMod((Math.floorDiv(code_1, p_1)), 2)) == 1) {
                    bits = bits + "1";
                } else {
                    bits = bits + "0";
                }
                j_1 = j_1 - 1;
            }
            i_2 = i_2 + 1;
        }
        return bits;
    }

    static String text_from_bits(String bits) {
        String text = "";
        long i_4 = 0L;
        while (i_4 < _runeLen(bits)) {
            long code_3 = 0L;
            long j_3 = 0L;
            while (j_3 < 8 && i_4 + j_3 < _runeLen(bits)) {
                code_3 = code_3 * 2;
                if ((bits.substring((int)((long)(i_4 + j_3)), (int)((long)(i_4 + j_3))+1).equals("1"))) {
                    code_3 = code_3 + 1;
                }
                j_3 = j_3 + 1;
            }
            text = text + String.valueOf(chr(code_3));
            i_4 = i_4 + 8;
        }
        return text;
    }

    static String bool_to_string(boolean b) {
        if (((Boolean)(b))) {
            return "True";
        }
        return "False";
    }

    static long[] string_to_bitlist(String s) {
        long[] res = ((long[])(new long[]{}));
        long i_6 = 0L;
        while (i_6 < _runeLen(s)) {
            if ((s.substring((int)((long)(i_6)), (int)((long)(i_6))+1).equals("1"))) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(1L)).toArray()));
            } else {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(0L)).toArray()));
            }
            i_6 = i_6 + 1;
        }
        return res;
    }

    static String bitlist_to_string(long[] bits) {
        String s = "";
        long i_8 = 0L;
        while (i_8 < bits.length) {
            if (bits[(int)((long)(i_8))] == 1) {
                s = s + "1";
            } else {
                s = s + "0";
            }
            i_8 = i_8 + 1;
        }
        return s;
    }

    static boolean is_power_of_two(long x) {
        if (x < 1) {
            return false;
        }
        long p_3 = 1L;
        while (p_3 < x) {
            p_3 = p_3 * 2;
        }
        return p_3 == x;
    }

    static boolean list_eq(long[] a, long[] b) {
        if (a.length != b.length) {
            return false;
        }
        long i_10 = 0L;
        while (i_10 < a.length) {
            if (a[(int)((long)(i_10))] != b[(int)((long)(i_10))]) {
                return false;
            }
            i_10 = i_10 + 1;
        }
        return true;
    }

    static long pow2(long e) {
        long res_1 = 1L;
        long i_12 = 0L;
        while (i_12 < e) {
            res_1 = res_1 * 2;
            i_12 = i_12 + 1;
        }
        return res_1;
    }

    static boolean has_bit(long n, long b) {
        long p_4 = pow2(b);
        if ((Math.floorMod((Math.floorDiv(n, p_4)), 2)) == 1) {
            return true;
        }
        return false;
    }

    static long[] hamming_encode(long r, long[] data_bits) {
        long total = r + data_bits.length;
        long[] data_ord_1 = ((long[])(new long[]{}));
        long cont_data_1 = 0L;
        long x_1 = 1L;
        while (x_1 <= total) {
            if (((Boolean)(is_power_of_two(x_1)))) {
                data_ord_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(data_ord_1), java.util.stream.LongStream.of(-1)).toArray()));
            } else {
                data_ord_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(data_ord_1), java.util.stream.LongStream.of(data_bits[(int)((long)(cont_data_1))])).toArray()));
                cont_data_1 = cont_data_1 + 1;
            }
            x_1 = x_1 + 1;
        }
        long[] parity_1 = ((long[])(new long[]{}));
        long bp_1 = 0L;
        while (bp_1 < r) {
            long cont_bo_1 = 0L;
            long j_5 = 0L;
            while (j_5 < data_ord_1.length) {
                long bit_1 = data_ord_1[(int)((long)(j_5))];
                if (bit_1 >= 0) {
                    long pos_1 = j_5 + 1;
                    if (((Boolean)(has_bit(pos_1, bp_1))) && bit_1 == 1) {
                        cont_bo_1 = cont_bo_1 + 1;
                    }
                }
                j_5 = j_5 + 1;
            }
            parity_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parity_1), java.util.stream.LongStream.of(Math.floorMod(cont_bo_1, 2))).toArray()));
            bp_1 = bp_1 + 1;
        }
        long[] result_1 = ((long[])(new long[]{}));
        long cont_bp_1 = 0L;
        long i_14 = 0L;
        while (i_14 < data_ord_1.length) {
            if (data_ord_1[(int)((long)(i_14))] < 0) {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of(parity_1[(int)((long)(cont_bp_1))])).toArray()));
                cont_bp_1 = cont_bp_1 + 1;
            } else {
                result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of(data_ord_1[(int)((long)(i_14))])).toArray()));
            }
            i_14 = i_14 + 1;
        }
        return result_1;
    }

    static DecodeResult hamming_decode(long r, long[] code) {
        long[] data_output = ((long[])(new long[]{}));
        long[] parity_received_1 = ((long[])(new long[]{}));
        long i_16 = 1L;
        long idx_3 = 0L;
        while (i_16 <= code.length) {
            if (((Boolean)(is_power_of_two(i_16)))) {
                parity_received_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parity_received_1), java.util.stream.LongStream.of(code[(int)((long)(idx_3))])).toArray()));
            } else {
                data_output = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(data_output), java.util.stream.LongStream.of(code[(int)((long)(idx_3))])).toArray()));
            }
            idx_3 = idx_3 + 1;
            i_16 = i_16 + 1;
        }
        long[] recomputed_1 = ((long[])(hamming_encode(r, ((long[])(data_output)))));
        long[] parity_calc_1 = ((long[])(new long[]{}));
        long j_7 = 0L;
        while (j_7 < recomputed_1.length) {
            if (((Boolean)(is_power_of_two(j_7 + 1)))) {
                parity_calc_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parity_calc_1), java.util.stream.LongStream.of(recomputed_1[(int)((long)(j_7))])).toArray()));
            }
            j_7 = j_7 + 1;
        }
        boolean ack_1 = list_eq(((long[])(parity_received_1)), ((long[])(parity_calc_1)));
        return new DecodeResult(data_output, ack_1);
    }

    static void main() {
        long sizePari = 4;
        long be_1 = 2;
        String text_2 = "Message01";
        String binary_1 = String.valueOf(text_to_bits(text_2));
        System.out.println("Text input in binary is '" + binary_1 + "'");
        long[] data_bits_1 = ((long[])(string_to_bitlist(binary_1)));
        long[] encoded_1 = ((long[])(hamming_encode(sizePari, ((long[])(data_bits_1)))));
        System.out.println("Data converted ----------> " + String.valueOf(bitlist_to_string(((long[])(encoded_1)))));
        DecodeResult decoded_1 = hamming_decode(sizePari, ((long[])(encoded_1)));
        System.out.println("Data receive ------------> " + String.valueOf(bitlist_to_string(((long[])(decoded_1.data)))) + " -- Data integrity: " + String.valueOf(bool_to_string(decoded_1.ack)));
        long[] corrupted_1 = ((long[])(new long[]{}));
        long i_18 = 0L;
        while (i_18 < encoded_1.length) {
            corrupted_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(corrupted_1), java.util.stream.LongStream.of(encoded_1[(int)((long)(i_18))])).toArray()));
            i_18 = i_18 + 1;
        }
        long pos_3 = be_1 - 1;
        if (corrupted_1[(int)((long)(pos_3))] == 0) {
corrupted_1[(int)((long)(pos_3))] = 1L;
        } else {
corrupted_1[(int)((long)(pos_3))] = 0L;
        }
        DecodeResult decoded_err_1 = hamming_decode(sizePari, ((long[])(corrupted_1)));
        System.out.println("Data receive (error) ----> " + String.valueOf(bitlist_to_string(((long[])(decoded_err_1.data)))) + " -- Data integrity: " + String.valueOf(bool_to_string(decoded_err_1.ack)));
    }
    public static void main(String[] args) {
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
