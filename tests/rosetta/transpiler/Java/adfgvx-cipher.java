public class Main {
    static String adfgvx = "ADFGVX";
    static String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    static String shuffleStr(String s) {
        String[] arr = new String[]{};
        int i = 0;
        while (i < s.length()) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(s.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        int j = arr.length - 1;
        while (j > 0) {
            int k = _now() % (j + 1);
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
        String shuffled = shuffleStr(alphabet);
        String[] labels = new String[]{};
        int li = 0;
        while (li < adfgvx.length()) {
            labels = java.util.stream.Stream.concat(java.util.Arrays.stream(labels), java.util.stream.Stream.of(adfgvx.substring(li, li + 1))).toArray(String[]::new);
            li = li + 1;
        }
        System.out.println("6 x 6 Polybius square:\n");
        System.out.println("  | A D F G V X");
        System.out.println("---------------");
        String[] p = new String[]{};
        int i = 0;
        while (i < 6) {
            String row = shuffled.substring(i * 6, (i + 1) * 6);
            p = java.util.stream.Stream.concat(java.util.Arrays.stream(p), java.util.stream.Stream.of(row)).toArray(String[]::new);
            String line = labels[i] + " | ";
            int j = 0;
            while (j < 6) {
                line = line + row.substring(j, j + 1) + " ";
                j = j + 1;
            }
            System.out.println(line);
            i = i + 1;
        }
        return p;
    }

    static String createKey(int n) {
        if (n < 7 || n > 12) {
            System.out.println("Key should be within 7 and 12 letters long.");
        }
        String pool = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        String key = "";
        int i = 0;
        while (i < n) {
            int idx = _now() % pool.length();
            key = key + pool.charAt(idx);
            pool = pool.substring(0, idx) + pool.substring(idx + 1, pool.length());
            i = i + 1;
        }
        System.out.println("\nThe key is " + key);
        return key;
    }

    static int[] orderKey(String key) {
        String[][] pairs = new String[][]{};
        int i = 0;
        while (i < key.length()) {
            pairs = appendObj(pairs, new String[]{key.substring(i, i + 1), i});
            i = i + 1;
        }
        int n = pairs.length;
        int m = 0;
        while (m < n) {
            int j = 0;
            while (j < n - 1) {
                if (pairs[j][0] > pairs[j + 1][0]) {
                    String[] tmp = pairs[j];
pairs[j] = pairs[j + 1];
pairs[j + 1] = tmp;
                }
                j = j + 1;
            }
            m = m + 1;
        }
        int[] res = new int[]{};
        i = 0;
        while (i < n) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(pairs[i][1])).toArray();
            i = i + 1;
        }
        return res;
    }

    static String encrypt(String[] polybius, String key, String plainText) {
        String[] labels = new String[]{};
        int li = 0;
        while (li < adfgvx.length()) {
            labels = java.util.stream.Stream.concat(java.util.Arrays.stream(labels), java.util.stream.Stream.of(adfgvx.substring(li, li + 1))).toArray(String[]::new);
            li = li + 1;
        }
        String temp = "";
        int i = 0;
        while (i < plainText.length()) {
            int r = 0;
            while (r < 6) {
                int c = 0;
                while (c < 6) {
                    if ((polybius[r].equals(plainText.substring(i, i + 1)))) {
                        temp = temp + java.util.Arrays.copyOfRange(labels, r, r + 1) + java.util.Arrays.copyOfRange(labels, c, c + 1);
                    }
                    c = c + 1;
                }
                r = r + 1;
            }
            i = i + 1;
        }
        int colLen = temp.length() / key.length();
        if (temp.length() % key.length() > 0) {
            colLen = colLen + 1;
        }
        string[][] table = new String[][]{};
        int rIdx = 0;
        while (rIdx < colLen) {
            String[] row = new String[]{};
            int j = 0;
            while (j < key.length()) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of("")).toArray(String[]::new);
                j = j + 1;
            }
            table = appendObj(table, row);
            rIdx = rIdx + 1;
        }
        int idx = 0;
        while (idx < temp.length()) {
            int row = idx / key.length();
            int col = idx % key.length();
table[row][col] = temp.substring(idx, idx + 1);
            idx = idx + 1;
        }
        int[] order = orderKey(key);
        String[] cols = new String[]{};
        int ci = 0;
        while (ci < key.length()) {
            String colStr = "";
            int ri = 0;
            while (ri < colLen) {
                colStr = colStr + table[ri][order[ci]];
                ri = ri + 1;
            }
            cols = java.util.stream.Stream.concat(java.util.Arrays.stream(cols), java.util.stream.Stream.of(colStr)).toArray(String[]::new);
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
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String decrypt(String[] polybius, String key, String cipherText) {
        String[] colStrs = new String[]{};
        int start = 0;
        int i = 0;
        while (i <= cipherText.length()) {
            if ((i == cipherText.length() || cipherText.charAt(i).equals(" "))) {
                colStrs = java.util.stream.Stream.concat(java.util.Arrays.stream(colStrs), java.util.stream.Stream.of(cipherText.substring(start, i))).toArray(String[]::new);
                start = i + 1;
            }
            i = i + 1;
        }
        int maxColLen = 0;
        i = 0;
        while (i < colStrs.length) {
            if (colStrs[i].length() > maxColLen) {
                maxColLen = colStrs[i].length();
            }
            i = i + 1;
        }
        string[][] cols = new String[][]{};
        i = 0;
        while (i < colStrs.length) {
            String s = colStrs[i];
            String[] ls = new String[]{};
            int j = 0;
            while (j < s.length()) {
                ls = java.util.stream.Stream.concat(java.util.Arrays.stream(ls), java.util.stream.Stream.of(s.substring(j, j + 1))).toArray(String[]::new);
                j = j + 1;
            }
            if (s.length() < maxColLen) {
                String[] pad = new String[]{};
                int k = 0;
                while (k < maxColLen) {
                    if (k < ls.length) {
                        pad = java.util.stream.Stream.concat(java.util.Arrays.stream(pad), java.util.stream.Stream.of(ls[k])).toArray(String[]::new);
                    } else {
                        pad = java.util.stream.Stream.concat(java.util.Arrays.stream(pad), java.util.stream.Stream.of("")).toArray(String[]::new);
                    }
                    k = k + 1;
                }
                cols = appendObj(cols, pad);
            } else {
                cols = appendObj(cols, ls);
            }
            i = i + 1;
        }
        string[][] table = new String[][]{};
        int r = 0;
        while (r < maxColLen) {
            String[] row = new String[]{};
            int c = 0;
            while (c < key.length()) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of("")).toArray(String[]::new);
                c = c + 1;
            }
            table = appendObj(table, row);
            r = r + 1;
        }
        int[] order = orderKey(key);
        r = 0;
        while (r < maxColLen) {
            int c = 0;
            while (c < key.length()) {
table[r][order[c]] = cols[c][r];
                c = c + 1;
            }
            r = r + 1;
        }
        String temp = "";
        r = 0;
        while (r < table.length) {
            int j = 0;
            while (j < table[r].length()) {
                temp = temp + table[r][j];
                j = j + 1;
            }
            r = r + 1;
        }
        String plainText = "";
        int idx = 0;
        while (idx < temp.length()) {
            int rIdx = indexOf(adfgvx, temp.substring(idx, idx + 1));
            int cIdx = indexOf(adfgvx, temp.substring(idx + 1, idx + 2));
            plainText = plainText + polybius[rIdx][cIdx];
            idx = idx + 2;
        }
        return plainText;
    }

    static void main() {
        String plainText = "ATTACKAT1200AM";
        String[] polybius = createPolybius();
        String key = createKey(9);
        System.out.println("\nPlaintext : " + plainText);
        String cipherText = encrypt(polybius, key, plainText);
        System.out.println("\nEncrypted : " + cipherText);
        String plainText2 = decrypt(polybius, key, cipherText);
        System.out.println("\nDecrypted : " + plainText2);
    }
    public static void main(String[] args) {
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
        return (int)System.currentTimeMillis();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
