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

    static int pow2(int n) {
        int res = 1;
        int i_1 = 0;
        while (i_1 < n) {
            res = res * 2;
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int bit_and(int a, int b) {
        int x = a;
        int y = b;
        int res_1 = 0;
        int bit = 1;
        int i_2 = 0;
        while (i_2 < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y, 2) == 1)) {
                res_1 = res_1 + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static int bit_or(int a, int b) {
        int x_1 = a;
        int y_1 = b;
        int res_2 = 0;
        int bit_1 = 1;
        int i_3 = 0;
        while (i_3 < 32) {
            int abit = Math.floorMod(x_1, 2);
            int bbit = Math.floorMod(y_1, 2);
            if (abit == 1 || bbit == 1) {
                res_2 = res_2 + bit_1;
            }
            x_1 = x_1 / 2;
            y_1 = y_1 / 2;
            bit_1 = bit_1 * 2;
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static int bit_xor(int a, int b) {
        int x_2 = a;
        int y_2 = b;
        int res_3 = 0;
        int bit_2 = 1;
        int i_4 = 0;
        while (i_4 < 32) {
            int abit_1 = Math.floorMod(x_2, 2);
            int bbit_1 = Math.floorMod(y_2, 2);
            if ((abit_1 == 1 && bbit_1 == 0) || (abit_1 == 0 && bbit_1 == 1)) {
                res_3 = res_3 + bit_2;
            }
            x_2 = x_2 / 2;
            y_2 = y_2 / 2;
            bit_2 = bit_2 * 2;
            i_4 = i_4 + 1;
        }
        return res_3;
    }

    static int bit_not(int a) {
        return (MOD - 1) - a;
    }

    static int rotate_left(int n, int b) {
        int left = Math.floorMod((n * pow2(b)), MOD);
        int right = n / pow2(32 - b);
        return Math.floorMod((left + right), MOD);
    }

    static String to_hex32(int n) {
        String digits = "0123456789abcdef";
        int num = n;
        String s = "";
        if (num == 0) {
            s = "0";
        }
        while (num > 0) {
            int d = Math.floorMod(num, 16);
            s = digits.substring(d, d + 1) + s;
            num = num / 16;
        }
        while (_runeLen(s) < 8) {
            s = "0" + s;
        }
        if (_runeLen(s) > 8) {
            s = s.substring(_runeLen(s) - 8, _runeLen(s));
        }
        return s;
    }

    static String sha1(String message) {
        int[] bytes = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 < _runeLen(message)) {
            bytes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bytes), java.util.stream.IntStream.of(ord(message.substring(i_5, i_5 + 1)))).toArray()));
            i_5 = i_5 + 1;
        }
        bytes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bytes), java.util.stream.IntStream.of(128)).toArray()));
        while (Math.floorMod((bytes.length + 8), 64) != 0) {
            bytes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bytes), java.util.stream.IntStream.of(0)).toArray()));
        }
        int bit_len = _runeLen(message) * 8;
        int[] len_bytes = ((int[])(new int[]{0, 0, 0, 0, 0, 0, 0, 0}));
        int bl = bit_len;
        int k = 7;
        while (k >= 0) {
len_bytes[k] = Math.floorMod(bl, 256);
            bl = bl / 256;
            k = k - 1;
        }
        int j = 0;
        while (j < 8) {
            bytes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bytes), java.util.stream.IntStream.of(len_bytes[j])).toArray()));
            j = j + 1;
        }
        int[][] blocks = ((int[][])(new int[][]{}));
        int pos = 0;
        while (pos < bytes.length) {
            int[] block = ((int[])(new int[]{}));
            int j2 = 0;
            while (j2 < 64) {
                block = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(block), java.util.stream.IntStream.of(bytes[pos + j2])).toArray()));
                j2 = j2 + 1;
            }
            blocks = ((int[][])(appendObj(blocks, block)));
            pos = pos + 64;
        }
        int h0 = 1732584193;
        int h1 = (int)4023233417L;
        int h2 = (int)2562383102L;
        int h3 = 271733878;
        int h4 = (int)3285377520L;
        int bindex = 0;
        while (bindex < blocks.length) {
            int[] block_1 = ((int[])(blocks[bindex]));
            int[] w = ((int[])(new int[]{}));
            int t = 0;
            while (t < 16) {
                int j3 = t * 4;
                int word = (((block_1[j3] * 256 + block_1[j3 + 1]) * 256 + block_1[j3 + 2]) * 256 + block_1[j3 + 3]);
                w = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(w), java.util.stream.IntStream.of(word)).toArray()));
                t = t + 1;
            }
            while (t < 80) {
                int tmp = bit_xor(bit_xor(bit_xor(w[t - 3], w[t - 8]), w[t - 14]), w[t - 16]);
                w = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(w), java.util.stream.IntStream.of(rotate_left(tmp, 1))).toArray()));
                t = t + 1;
            }
            int a = h0;
            int b = h1;
            int c = h2;
            int d_1 = h3;
            int e = h4;
            int i2 = 0;
            while (i2 < 80) {
                int f = 0;
                int kconst = 0;
                if (i2 < 20) {
                    f = bit_or(bit_and(b, c), bit_and(bit_not(b), d_1));
                    kconst = 1518500249;
                } else                 if (i2 < 40) {
                    f = bit_xor(bit_xor(b, c), d_1);
                    kconst = 1859775393;
                } else                 if (i2 < 60) {
                    f = bit_or(bit_or(bit_and(b, c), bit_and(b, d_1)), bit_and(c, d_1));
                    kconst = (int)2400959708L;
                } else {
                    f = bit_xor(bit_xor(b, c), d_1);
                    kconst = (int)3395469782L;
                }
                int temp = Math.floorMod((rotate_left(a, 5) + f + e + kconst + w[i2]), MOD);
                e = d_1;
                d_1 = c;
                c = rotate_left(b, 30);
                b = a;
                a = temp;
                i2 = i2 + 1;
            }
            h0 = Math.floorMod((h0 + a), MOD);
            h1 = Math.floorMod((h1 + b), MOD);
            h2 = Math.floorMod((h2 + c), MOD);
            h3 = Math.floorMod((h3 + d_1), MOD);
            h4 = Math.floorMod((h4 + e), MOD);
            bindex = bindex + 1;
        }
        return String.valueOf(String.valueOf(String.valueOf(String.valueOf(to_hex32(h0)) + String.valueOf(to_hex32(h1))) + String.valueOf(to_hex32(h2))) + String.valueOf(to_hex32(h3))) + String.valueOf(to_hex32(h4));
    }

    static void main() {
        System.out.println(sha1("Test String"));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            MOD = (int)4294967296L;
            ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
