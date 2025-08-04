public class Main {
    static String adfgvx;
    static String alphabet;

    static String shuffleStr(String s) {
        String[] arr = ((String[])(new String[]{}));
        int i = 0;
        while (i < _runeLen(s)) {
            arr = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(s.substring(i, i + 1))).toArray(String[]::new)));
            i = i + 1;
        }
        int j = arr.length - 1;
        while (j > 0) {
            int k = Math.floorMod(_now(), (j + 1));
            String tmp = arr[j];
arr[j] = arr[k];
arr[k] = tmp;
            j = j - 1;
        }
        String out = "";
        i = 0;
        while (i < arr.length) {
            out = out + arr[i];
            i = i + 1;
        }
        return out;
    }

    static String[] createPolybius() {
        String shuffled = String.valueOf(shuffleStr(alphabet));
        String[] labels = ((String[])(new String[]{}));
        int li = 0;
        while (li < _runeLen(adfgvx)) {
            labels = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(labels), java.util.stream.Stream.of(adfgvx.substring(li, li + 1))).toArray(String[]::new)));
            li = li + 1;
        }
        System.out.println("6 x 6 Polybius square:\n");
        System.out.println("  | A D F G V X");
        System.out.println("---------------");
        String[] p = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < 6) {
            String row = shuffled.substring(i_1 * 6, (i_1 + 1) * 6);
            p = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(p), java.util.stream.Stream.of(row)).toArray(String[]::new)));
            String line = labels[i_1] + " | ";
            int j_1 = 0;
            while (j_1 < 6) {
                line = line + row.substring(j_1, j_1 + 1) + " ";
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
        return p;
    }

    static String createKey(int n) {
        if (n < 7 || n > 12) {
            System.out.println("Key should be within 7 and 12 letters long.");
        }
        String pool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        String key = "";
        int i_2 = 0;
        while (i_2 < n) {
            int idx = Math.floorMod(_now(), _runeLen(pool));
            key = key + pool.substring(idx, idx+1);
            pool = pool.substring(0, idx) + pool.substring(idx + 1, _runeLen(pool));
            i_2 = i_2 + 1;
        }
        System.out.println("\nThe key is " + key);
        return key;
    }

    static int[] orderKey(String key) {
        Object[][] pairs = ((Object[][])(new Object[][]{}));
        int i_3 = 0;
        while (i_3 < _runeLen(key)) {
            pairs = ((Object[][])(appendObj(pairs, new Object[]{key.substring(i_3, i_3 + 1), i_3})));
            i_3 = i_3 + 1;
        }
        int n = pairs.length;
        int m = 0;
        while (m < n) {
            int j_2 = 0;
            while (j_2 < n - 1) {
                if (String.valueOf(pairs[j_2][0]).compareTo(String.valueOf(pairs[j_2 + 1][0])) > 0) {
                    Object[] tmp_1 = ((Object[])(pairs[j_2]));
pairs[j_2] = ((Object[])(pairs[j_2 + 1]));
pairs[j_2 + 1] = ((Object[])(tmp_1));
                }
                j_2 = j_2 + 1;
            }
            m = m + 1;
        }
        int[] res = ((int[])(new int[]{}));
        i_3 = 0;
        while (i_3 < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(((int)(pairs[i_3][1])))).toArray()));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static String encrypt(String[] polybius, String key, String plainText) {
        String[] labels_1 = ((String[])(new String[]{}));
        int li_1 = 0;
        while (li_1 < _runeLen(adfgvx)) {
            labels_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(labels_1), java.util.stream.Stream.of(adfgvx.substring(li_1, li_1 + 1))).toArray(String[]::new)));
            li_1 = li_1 + 1;
        }
        String temp = "";
        int i_4 = 0;
        while (i_4 < _runeLen(plainText)) {
            int r = 0;
            while (r < 6) {
                int c = 0;
                while (c < 6) {
                    if ((polybius[r].substring(c, c + 1).equals(plainText.substring(i_4, i_4 + 1)))) {
                        temp = temp + String.join("", java.util.Arrays.copyOfRange(labels_1, r, r + 1)) + String.join("", java.util.Arrays.copyOfRange(labels_1, c, c + 1));
                    }
                    c = c + 1;
                }
                r = r + 1;
            }
            i_4 = i_4 + 1;
        }
        int colLen = _runeLen(temp) / _runeLen(key);
        if (Math.floorMod(_runeLen(temp), _runeLen(key)) > 0) {
            colLen = colLen + 1;
        }
        String[][] table = ((String[][])(new String[][]{}));
        int rIdx = 0;
        while (rIdx < colLen) {
            String[] row_1 = ((String[])(new String[]{}));
            int j_3 = 0;
            while (j_3 < _runeLen(key)) {
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of("")).toArray(String[]::new)));
                j_3 = j_3 + 1;
            }
            table = ((String[][])(appendObj(table, row_1)));
            rIdx = rIdx + 1;
        }
        int idx_1 = 0;
        while (idx_1 < _runeLen(temp)) {
            int row_2 = idx_1 / _runeLen(key);
            int col = Math.floorMod(idx_1, _runeLen(key));
table[row_2][col] = temp.substring(idx_1, idx_1 + 1);
            idx_1 = idx_1 + 1;
        }
        int[] order = ((int[])(orderKey(key)));
        String[] cols = ((String[])(new String[]{}));
        int ci = 0;
        while (ci < _runeLen(key)) {
            String colStr = "";
            int ri = 0;
            while (ri < colLen) {
                colStr = colStr + table[ri][order[ci]];
                ri = ri + 1;
            }
            cols = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols), java.util.stream.Stream.of(colStr)).toArray(String[]::new)));
            ci = ci + 1;
        }
        String result = "";
        ci = 0;
        while (ci < cols.length) {
            result = result + cols[ci];
            if (ci < cols.length - 1) {
                result = result + " ";
            }
            ci = ci + 1;
        }
        return result;
    }

    static int indexOf(String s, String ch) {
        int i_5 = 0;
        while (i_5 < _runeLen(s)) {
            if ((s.substring(i_5, i_5 + 1).equals(ch))) {
                return i_5;
            }
            i_5 = i_5 + 1;
        }
        return -1;
    }

    static String decrypt(String[] polybius, String key, String cipherText) {
        String[] colStrs = ((String[])(new String[]{}));
        int start = 0;
        int i_6 = 0;
        while (i_6 <= _runeLen(cipherText)) {
            if (i_6 == _runeLen(cipherText) || (cipherText.substring(i_6, i_6+1).equals(" "))) {
                colStrs = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(colStrs), java.util.stream.Stream.of(cipherText.substring(start, i_6))).toArray(String[]::new)));
                start = i_6 + 1;
            }
            i_6 = i_6 + 1;
        }
        int maxColLen = 0;
        i_6 = 0;
        while (i_6 < colStrs.length) {
            if (colStrs[i_6].length() > maxColLen) {
                maxColLen = colStrs[i_6].length();
            }
            i_6 = i_6 + 1;
        }
        String[][] cols_1 = ((String[][])(new String[][]{}));
        i_6 = 0;
        while (i_6 < colStrs.length) {
            String s = colStrs[i_6];
            String[] ls = ((String[])(new String[]{}));
            int j_4 = 0;
            while (j_4 < _runeLen(s)) {
                ls = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ls), java.util.stream.Stream.of(s.substring(j_4, j_4 + 1))).toArray(String[]::new)));
                j_4 = j_4 + 1;
            }
            if (_runeLen(s) < maxColLen) {
                String[] pad = ((String[])(new String[]{}));
                int k_1 = 0;
                while (k_1 < maxColLen) {
                    if (k_1 < ls.length) {
                        pad = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pad), java.util.stream.Stream.of(ls[k_1])).toArray(String[]::new)));
                    } else {
                        pad = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pad), java.util.stream.Stream.of("")).toArray(String[]::new)));
                    }
                    k_1 = k_1 + 1;
                }
                cols_1 = ((String[][])(appendObj(cols_1, pad)));
            } else {
                cols_1 = ((String[][])(appendObj(cols_1, ls)));
            }
            i_6 = i_6 + 1;
        }
        String[][] table_1 = ((String[][])(new String[][]{}));
        int r_1 = 0;
        while (r_1 < maxColLen) {
            String[] row_3 = ((String[])(new String[]{}));
            int c_1 = 0;
            while (c_1 < _runeLen(key)) {
                row_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_3), java.util.stream.Stream.of("")).toArray(String[]::new)));
                c_1 = c_1 + 1;
            }
            table_1 = ((String[][])(appendObj(table_1, row_3)));
            r_1 = r_1 + 1;
        }
        int[] order_1 = ((int[])(orderKey(key)));
        r_1 = 0;
        while (r_1 < maxColLen) {
            int c_2 = 0;
            while (c_2 < _runeLen(key)) {
table_1[r_1][order_1[c_2]] = cols_1[c_2][r_1];
                c_2 = c_2 + 1;
            }
            r_1 = r_1 + 1;
        }
        String temp_1 = "";
        r_1 = 0;
        while (r_1 < table_1.length) {
            int j_5 = 0;
            while (j_5 < table_1[r_1].length) {
                temp_1 = temp_1 + table_1[r_1][j_5];
                j_5 = j_5 + 1;
            }
            r_1 = r_1 + 1;
        }
        String plainText = "";
        int idx_2 = 0;
        while (idx_2 < _runeLen(temp_1)) {
            int rIdx_1 = indexOf(adfgvx, temp_1.substring(idx_2, idx_2 + 1));
            int cIdx = indexOf(adfgvx, temp_1.substring(idx_2 + 1, idx_2 + 2));
            plainText = plainText + polybius[rIdx_1].substring(cIdx, cIdx+1);
            idx_2 = idx_2 + 2;
        }
        return plainText;
    }

    static void main() {
        String plainText_1 = "ATTACKAT1200AM";
        String[] polybius = ((String[])(createPolybius()));
        String key_1 = String.valueOf(createKey(9));
        System.out.println("\nPlaintext : " + plainText_1);
        String cipherText = String.valueOf(encrypt(((String[])(polybius)), key_1, plainText_1));
        System.out.println("\nEncrypted : " + cipherText);
        String plainText2 = String.valueOf(decrypt(((String[])(polybius)), key_1, cipherText));
        System.out.println("\nDecrypted : " + plainText2);
    }
    public static void main(String[] args) {
        adfgvx = "ADFGVX";
        alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
