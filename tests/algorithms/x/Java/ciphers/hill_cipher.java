public class Main {
    static String KEY_STRING;
    static int[][] key;

    static int mod36(int n) {
        int r = Math.floorMod(n, 36);
        if (r < 0) {
            r = r + 36;
        }
        return r;
    }

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = y;
            y = Math.floorMod(x, y);
            x = t;
        }
        if (x < 0) {
            x = -x;
        }
        return x;
    }

    static int replace_letters(String letter) {
        int i = 0;
        while (i < _runeLen(KEY_STRING)) {
            if ((KEY_STRING.substring(i, i+1).equals(letter))) {
                return i;
            }
            i = i + 1;
        }
        return 0;
    }

    static String replace_digits(int num) {
        int idx = mod36(num);
        return KEY_STRING.substring(idx, idx+1);
    }

    static String to_upper(String c) {
        String lower = "abcdefghijklmnopqrstuvwxyz";
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int i_1 = 0;
        while (i_1 < _runeLen(lower)) {
            if ((c.equals(lower.substring(i_1, i_1+1)))) {
                return upper.substring(i_1, i_1+1);
            }
            i_1 = i_1 + 1;
        }
        return c;
    }

    static String process_text(String text, int break_key) {
        String[] chars = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < _runeLen(text)) {
            String c = String.valueOf(to_upper(text.substring(i_2, i_2+1)));
            int j = 0;
            boolean ok = false;
            while (j < _runeLen(KEY_STRING)) {
                if ((KEY_STRING.substring(j, j+1).equals(c))) {
                    ok = true;
                    break;
                }
                j = j + 1;
            }
            if (ok) {
                chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(c)).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        if (chars.length == 0) {
            return "";
        }
        String last = chars[chars.length - 1];
        while (Math.floorMod(chars.length, break_key) != 0) {
            chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(last)).toArray(String[]::new)));
        }
        String res = "";
        int k = 0;
        while (k < chars.length) {
            res = res + chars[k];
            k = k + 1;
        }
        return res;
    }

    static int[][] matrix_minor(int[][] m, int row, int col) {
        int[][] res_1 = ((int[][])(new int[][]{}));
        int i_3 = 0;
        while (i_3 < m.length) {
            if (i_3 != row) {
                int[] r_1 = ((int[])(new int[]{}));
                int j_1 = 0;
                while (j_1 < m[i_3].length) {
                    if (j_1 != col) {
                        r_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r_1), java.util.stream.IntStream.of(m[i_3][j_1])).toArray()));
                    }
                    j_1 = j_1 + 1;
                }
                res_1 = ((int[][])(appendObj(res_1, r_1)));
            }
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static int determinant(int[][] m) {
        int n = m.length;
        if (n == 1) {
            return m[0][0];
        }
        if (n == 2) {
            return m[0][0] * m[1][1] - m[0][1] * m[1][0];
        }
        int det = 0;
        int col = 0;
        while (col < n) {
            int[][] minor_mat = ((int[][])(matrix_minor(((int[][])(m)), 0, col)));
            int sign = 1;
            if (Math.floorMod(col, 2) == 1) {
                sign = -1;
            }
            det = det + sign * m[0][col] * determinant(((int[][])(minor_mat)));
            col = col + 1;
        }
        return det;
    }

    static int[][] cofactor_matrix(int[][] m) {
        int n_1 = m.length;
        int[][] res_2 = ((int[][])(new int[][]{}));
        int i_4 = 0;
        while (i_4 < n_1) {
            int[] row = ((int[])(new int[]{}));
            int j_2 = 0;
            while (j_2 < n_1) {
                int[][] minor_mat_1 = ((int[][])(matrix_minor(((int[][])(m)), i_4, j_2)));
                int det_minor = determinant(((int[][])(minor_mat_1)));
                int sign_1 = 1;
                if (Math.floorMod((i_4 + j_2), 2) == 1) {
                    sign_1 = -1;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(sign_1 * det_minor)).toArray()));
                j_2 = j_2 + 1;
            }
            res_2 = ((int[][])(appendObj(res_2, row)));
            i_4 = i_4 + 1;
        }
        return res_2;
    }

    static int[][] transpose(int[][] m) {
        int rows = m.length;
        int cols = m[0].length;
        int[][] res_3 = ((int[][])(new int[][]{}));
        int j_3 = 0;
        while (j_3 < cols) {
            int[] row_1 = ((int[])(new int[]{}));
            int i_5 = 0;
            while (i_5 < rows) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(m[i_5][j_3])).toArray()));
                i_5 = i_5 + 1;
            }
            res_3 = ((int[][])(appendObj(res_3, row_1)));
            j_3 = j_3 + 1;
        }
        return res_3;
    }

    static int[][] matrix_mod(int[][] m) {
        int[][] res_4 = ((int[][])(new int[][]{}));
        int i_6 = 0;
        while (i_6 < m.length) {
            int[] row_2 = ((int[])(new int[]{}));
            int j_4 = 0;
            while (j_4 < m[i_6].length) {
                row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(mod36(m[i_6][j_4]))).toArray()));
                j_4 = j_4 + 1;
            }
            res_4 = ((int[][])(appendObj(res_4, row_2)));
            i_6 = i_6 + 1;
        }
        return res_4;
    }

    static int[][] scalar_matrix_mult(int s, int[][] m) {
        int[][] res_5 = ((int[][])(new int[][]{}));
        int i_7 = 0;
        while (i_7 < m.length) {
            int[] row_3 = ((int[])(new int[]{}));
            int j_5 = 0;
            while (j_5 < m[i_7].length) {
                row_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_3), java.util.stream.IntStream.of(mod36(s * m[i_7][j_5]))).toArray()));
                j_5 = j_5 + 1;
            }
            res_5 = ((int[][])(appendObj(res_5, row_3)));
            i_7 = i_7 + 1;
        }
        return res_5;
    }

    static int[][] adjugate(int[][] m) {
        int[][] cof = ((int[][])(cofactor_matrix(((int[][])(m)))));
        int n_2 = cof.length;
        int[][] res_6 = ((int[][])(new int[][]{}));
        int i_8 = 0;
        while (i_8 < n_2) {
            int[] row_4 = ((int[])(new int[]{}));
            int j_6 = 0;
            while (j_6 < n_2) {
                row_4 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_4), java.util.stream.IntStream.of(cof[j_6][i_8])).toArray()));
                j_6 = j_6 + 1;
            }
            res_6 = ((int[][])(appendObj(res_6, row_4)));
            i_8 = i_8 + 1;
        }
        return res_6;
    }

    static int[] multiply_matrix_vector(int[][] m, int[] v) {
        int n_3 = m.length;
        int[] res_7 = ((int[])(new int[]{}));
        int i_9 = 0;
        while (i_9 < n_3) {
            int sum = 0;
            int j_7 = 0;
            while (j_7 < n_3) {
                sum = sum + m[i_9][j_7] * v[j_7];
                j_7 = j_7 + 1;
            }
            res_7 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_7), java.util.stream.IntStream.of(mod36(sum))).toArray()));
            i_9 = i_9 + 1;
        }
        return res_7;
    }

    static int[][] inverse_key(int[][] key) {
        int det_val = determinant(((int[][])(key)));
        int det_mod = mod36(det_val);
        int det_inv = 0;
        int i_10 = 0;
        while (i_10 < 36) {
            if (Math.floorMod((det_mod * i_10), 36) == 1) {
                det_inv = i_10;
                break;
            }
            i_10 = i_10 + 1;
        }
        int[][] adj = ((int[][])(adjugate(((int[][])(key)))));
        int[][] tmp = ((int[][])(scalar_matrix_mult(det_inv, ((int[][])(adj)))));
        int[][] res_8 = ((int[][])(matrix_mod(((int[][])(tmp)))));
        return res_8;
    }

    static String hill_encrypt(int[][] key, String text) {
        int break_key = key.length;
        String processed = String.valueOf(process_text(text, break_key));
        String encrypted = "";
        int i_11 = 0;
        while (i_11 < _runeLen(processed)) {
            int[] vec = ((int[])(new int[]{}));
            int j_8 = 0;
            while (j_8 < break_key) {
                vec = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vec), java.util.stream.IntStream.of(replace_letters(processed.substring(i_11 + j_8, i_11 + j_8+1)))).toArray()));
                j_8 = j_8 + 1;
            }
            int[] enc_vec = ((int[])(multiply_matrix_vector(((int[][])(key)), ((int[])(vec)))));
            int k_1 = 0;
            while (k_1 < break_key) {
                encrypted = encrypted + String.valueOf(replace_digits(enc_vec[k_1]));
                k_1 = k_1 + 1;
            }
            i_11 = i_11 + break_key;
        }
        return encrypted;
    }

    static String hill_decrypt(int[][] key, String text) {
        int break_key_1 = key.length;
        int[][] decrypt_key = ((int[][])(inverse_key(((int[][])(key)))));
        String processed_1 = String.valueOf(process_text(text, break_key_1));
        String decrypted = "";
        int i_12 = 0;
        while (i_12 < _runeLen(processed_1)) {
            int[] vec_1 = ((int[])(new int[]{}));
            int j_9 = 0;
            while (j_9 < break_key_1) {
                vec_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vec_1), java.util.stream.IntStream.of(replace_letters(processed_1.substring(i_12 + j_9, i_12 + j_9+1)))).toArray()));
                j_9 = j_9 + 1;
            }
            int[] dec_vec = ((int[])(multiply_matrix_vector(((int[][])(decrypt_key)), ((int[])(vec_1)))));
            int k_2 = 0;
            while (k_2 < break_key_1) {
                decrypted = decrypted + String.valueOf(replace_digits(dec_vec[k_2]));
                k_2 = k_2 + 1;
            }
            i_12 = i_12 + break_key_1;
        }
        return decrypted;
    }
    public static void main(String[] args) {
        KEY_STRING = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        key = ((int[][])(new int[][]{new int[]{2, 5}, new int[]{1, 6}}));
        System.out.println(hill_encrypt(((int[][])(key)), "testing hill cipher"));
        System.out.println(hill_encrypt(((int[][])(key)), "hello"));
        System.out.println(hill_decrypt(((int[][])(key)), "WHXYJOLM9C6XT085LL"));
        System.out.println(hill_decrypt(((int[][])(key)), "85FF00"));
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
