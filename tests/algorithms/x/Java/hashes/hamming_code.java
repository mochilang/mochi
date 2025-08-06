public class Main {
    static class DecodeResult {
        int[] data;
        boolean ack;
        DecodeResult(int[] data, boolean ack) {
            this.data = data;
            this.ack = ack;
        }
        DecodeResult() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'ack': %s}", String.valueOf(data), String.valueOf(ack));
        }
    }


    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = index_of(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = index_of(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper_1.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower_1.substring(n - 97, n - 96);
        }
        return "?";
    }

    static String text_to_bits(String text) {
        String bits = "";
        int i_1 = 0;
        while (i_1 < _runeLen(text)) {
            int code = ord(text.substring(i_1, i_1+1));
            int j = 7;
            while (j >= 0) {
                int p = pow2(j);
                if ((Math.floorMod((code / p), 2)) == 1) {
                    bits = bits + "1";
                } else {
                    bits = bits + "0";
                }
                j = j - 1;
            }
            i_1 = i_1 + 1;
        }
        return bits;
    }

    static String text_from_bits(String bits) {
        String text = "";
        int i_2 = 0;
        while (i_2 < _runeLen(bits)) {
            int code_1 = 0;
            int j_1 = 0;
            while (j_1 < 8 && i_2 + j_1 < _runeLen(bits)) {
                code_1 = code_1 * 2;
                if ((bits.substring(i_2 + j_1, i_2 + j_1+1).equals("1"))) {
                    code_1 = code_1 + 1;
                }
                j_1 = j_1 + 1;
            }
            text = text + String.valueOf(chr(code_1));
            i_2 = i_2 + 8;
        }
        return text;
    }

    static String bool_to_string(boolean b) {
        if (((Boolean)(b))) {
            return "True";
        }
        return "False";
    }

    static int[] string_to_bitlist(String s) {
        int[] res = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            if ((s.substring(i_3, i_3+1).equals("1"))) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(1)).toArray()));
            } else {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(0)).toArray()));
            }
            i_3 = i_3 + 1;
        }
        return res;
    }

    static String bitlist_to_string(int[] bits) {
        String s = "";
        int i_4 = 0;
        while (i_4 < bits.length) {
            if (bits[i_4] == 1) {
                s = s + "1";
            } else {
                s = s + "0";
            }
            i_4 = i_4 + 1;
        }
        return s;
    }

    static boolean is_power_of_two(int x) {
        if (x < 1) {
            return false;
        }
        int p_1 = 1;
        while (p_1 < x) {
            p_1 = p_1 * 2;
        }
        return p_1 == x;
    }

    static boolean list_eq(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_5 = 0;
        while (i_5 < a.length) {
            if (a[i_5] != b[i_5]) {
                return false;
            }
            i_5 = i_5 + 1;
        }
        return true;
    }

    static int pow2(int e) {
        int res_1 = 1;
        int i_6 = 0;
        while (i_6 < e) {
            res_1 = res_1 * 2;
            i_6 = i_6 + 1;
        }
        return res_1;
    }

    static boolean has_bit(int n, int b) {
        int p_2 = pow2(b);
        if ((Math.floorMod((n / p_2), 2)) == 1) {
            return true;
        }
        return false;
    }

    static int[] hamming_encode(int r, int[] data_bits) {
        int total = r + data_bits.length;
        int[] data_ord = ((int[])(new int[]{}));
        int cont_data = 0;
        int x = 1;
        while (x <= total) {
            if (((Boolean)(is_power_of_two(x)))) {
                data_ord = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(data_ord), java.util.stream.IntStream.of(-1)).toArray()));
            } else {
                data_ord = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(data_ord), java.util.stream.IntStream.of(data_bits[cont_data])).toArray()));
                cont_data = cont_data + 1;
            }
            x = x + 1;
        }
        int[] parity = ((int[])(new int[]{}));
        int bp = 0;
        while (bp < r) {
            int cont_bo = 0;
            int j_2 = 0;
            while (j_2 < data_ord.length) {
                int bit = data_ord[j_2];
                if (bit >= 0) {
                    int pos = j_2 + 1;
                    if (((Boolean)(has_bit(pos, bp))) && bit == 1) {
                        cont_bo = cont_bo + 1;
                    }
                }
                j_2 = j_2 + 1;
            }
            parity = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parity), java.util.stream.IntStream.of(Math.floorMod(cont_bo, 2))).toArray()));
            bp = bp + 1;
        }
        int[] result = ((int[])(new int[]{}));
        int cont_bp = 0;
        int i_7 = 0;
        while (i_7 < data_ord.length) {
            if (data_ord[i_7] < 0) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(parity[cont_bp])).toArray()));
                cont_bp = cont_bp + 1;
            } else {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(data_ord[i_7])).toArray()));
            }
            i_7 = i_7 + 1;
        }
        return result;
    }

    static DecodeResult hamming_decode(int r, int[] code) {
        int[] data_output = ((int[])(new int[]{}));
        int[] parity_received = ((int[])(new int[]{}));
        int i_8 = 1;
        int idx_1 = 0;
        while (i_8 <= code.length) {
            if (((Boolean)(is_power_of_two(i_8)))) {
                parity_received = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parity_received), java.util.stream.IntStream.of(code[idx_1])).toArray()));
            } else {
                data_output = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(data_output), java.util.stream.IntStream.of(code[idx_1])).toArray()));
            }
            idx_1 = idx_1 + 1;
            i_8 = i_8 + 1;
        }
        int[] recomputed = ((int[])(hamming_encode(r, ((int[])(data_output)))));
        int[] parity_calc = ((int[])(new int[]{}));
        int j_3 = 0;
        while (j_3 < recomputed.length) {
            if (((Boolean)(is_power_of_two(j_3 + 1)))) {
                parity_calc = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parity_calc), java.util.stream.IntStream.of(recomputed[j_3])).toArray()));
            }
            j_3 = j_3 + 1;
        }
        boolean ack = list_eq(((int[])(parity_received)), ((int[])(parity_calc)));
        return new DecodeResult(data_output, ack);
    }

    static void main() {
        int sizePari = 4;
        int be = 2;
        String text_1 = "Message01";
        String binary = String.valueOf(text_to_bits(text_1));
        System.out.println("Text input in binary is '" + binary + "'");
        int[] data_bits = ((int[])(string_to_bitlist(binary)));
        int[] encoded = ((int[])(hamming_encode(sizePari, ((int[])(data_bits)))));
        System.out.println("Data converted ----------> " + String.valueOf(bitlist_to_string(((int[])(encoded)))));
        DecodeResult decoded = hamming_decode(sizePari, ((int[])(encoded)));
        System.out.println("Data receive ------------> " + String.valueOf(bitlist_to_string(((int[])(decoded.data)))) + " -- Data integrity: " + String.valueOf(bool_to_string(decoded.ack)));
        int[] corrupted = ((int[])(new int[]{}));
        int i_9 = 0;
        while (i_9 < encoded.length) {
            corrupted = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(corrupted), java.util.stream.IntStream.of(encoded[i_9])).toArray()));
            i_9 = i_9 + 1;
        }
        int pos_1 = be - 1;
        if (corrupted[pos_1] == 0) {
corrupted[pos_1] = 1;
        } else {
corrupted[pos_1] = 0;
        }
        DecodeResult decoded_err = hamming_decode(sizePari, ((int[])(corrupted)));
        System.out.println("Data receive (error) ----> " + String.valueOf(bitlist_to_string(((int[])(decoded_err.data)))) + " -- Data integrity: " + String.valueOf(bool_to_string(decoded_err.ack)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
