public class Main {
    static class Cipher {
        String[] passcode;
        String[] key_list;
        int shift_key;
        Cipher(String[] passcode, String[] key_list, int shift_key) {
            this.passcode = passcode;
            this.key_list = key_list;
            this.shift_key = shift_key;
        }
        Cipher() {}
        @Override public String toString() {
            return String.format("{'passcode': %s, 'key_list': %s, 'shift_key': %s}", String.valueOf(passcode), String.valueOf(key_list), String.valueOf(shift_key));
        }
    }

    static Cipher ssc;
    static String encoded_1;

    static int ord(String ch) {
        String digits = "0123456789";
        int i = 0;
        while (i < _runeLen(digits)) {
            if ((_substr(digits, i, i + 1).equals(ch))) {
                return 48 + i;
            }
            i = i + 1;
        }
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        i = 0;
        while (i < _runeLen(upper)) {
            if ((_substr(upper, i, i + 1).equals(ch))) {
                return 65 + i;
            }
            i = i + 1;
        }
        String lower = "abcdefghijklmnopqrstuvwxyz";
        i = 0;
        while (i < _runeLen(lower)) {
            if ((_substr(lower, i, i + 1).equals(ch))) {
                return 97 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static int[] neg_pos(int[] iterlist) {
        int i_1 = 1;
        while (i_1 < iterlist.length) {
iterlist[i_1] = -iterlist[i_1];
            i_1 = i_1 + 2;
        }
        return iterlist;
    }

    static String[] passcode_creator() {
        String choices = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        int seed = _now();
        int length = 10 + (Math.floorMod(seed, 11));
        String[] password = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < length) {
            seed = Math.floorMod((seed * 1103515245 + 12345), 2147483647);
            int idx = Math.floorMod(seed, _runeLen(choices));
            password = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(password), java.util.stream.Stream.of(_substr(choices, idx, idx + 1))).toArray(String[]::new)));
            i_2 = i_2 + 1;
        }
        return password;
    }

    static String[] unique_sorted(String[] chars) {
        String[] uniq = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < chars.length) {
            String ch = chars[i_3];
            if (!(java.util.Arrays.asList(uniq).contains(ch))) {
                uniq = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(uniq), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        int j = 0;
        while (j < uniq.length) {
            int k = j + 1;
            int min_idx = j;
            while (k < uniq.length) {
                if ((uniq[k].compareTo(uniq[min_idx]) < 0)) {
                    min_idx = k;
                }
                k = k + 1;
            }
            if (min_idx != j) {
                String tmp = uniq[j];
uniq[j] = uniq[min_idx];
uniq[min_idx] = tmp;
            }
            j = j + 1;
        }
        return uniq;
    }

    static String[] make_key_list(String[] passcode) {
        String key_list_options = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n";
        String[] breakpoints = ((String[])(unique_sorted(((String[])(passcode)))));
        String[] keys_l = ((String[])(new String[]{}));
        String[] temp_list = ((String[])(new String[]{}));
        int i_4 = 0;
        while (i_4 < _runeLen(key_list_options)) {
            String ch_1 = _substr(key_list_options, i_4, i_4 + 1);
            temp_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_list), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            if (java.util.Arrays.asList(breakpoints).contains(ch_1) || i_4 == _runeLen(key_list_options) - 1) {
                int k_1 = temp_list.length - 1;
                while (k_1 >= 0) {
                    keys_l = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_l), java.util.stream.Stream.of(temp_list[k_1])).toArray(String[]::new)));
                    k_1 = k_1 - 1;
                }
                temp_list = ((String[])(new String[]{}));
            }
            i_4 = i_4 + 1;
        }
        return keys_l;
    }

    static int make_shift_key(String[] passcode) {
        int[] codes = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 < passcode.length) {
            codes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(codes), java.util.stream.IntStream.of(ord(passcode[i_5]))).toArray()));
            i_5 = i_5 + 1;
        }
        codes = ((int[])(neg_pos(((int[])(codes)))));
        int total = 0;
        i_5 = 0;
        while (i_5 < codes.length) {
            total = total + codes[i_5];
            i_5 = i_5 + 1;
        }
        if (total > 0) {
            return total;
        }
        return passcode.length;
    }

    static Cipher new_cipher(String passcode_str) {
        String[] passcode = ((String[])(new String[]{}));
        if (_runeLen(passcode_str) == 0) {
            passcode = ((String[])(passcode_creator()));
        } else {
            int i_6 = 0;
            while (i_6 < _runeLen(passcode_str)) {
                passcode = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(passcode), java.util.stream.Stream.of(_substr(passcode_str, i_6, i_6 + 1))).toArray(String[]::new)));
                i_6 = i_6 + 1;
            }
        }
        String[] key_list = ((String[])(make_key_list(((String[])(passcode)))));
        int shift_key = make_shift_key(((String[])(passcode)));
        return new Cipher(passcode, key_list, shift_key);
    }

    static int index_of(String[] lst, String ch) {
        int i_7 = 0;
        while (i_7 < lst.length) {
            if ((lst[i_7].equals(ch))) {
                return i_7;
            }
            i_7 = i_7 + 1;
        }
        return -1;
    }

    static String encrypt(Cipher c, String plaintext) {
        String encoded = "";
        int i_8 = 0;
        int n = c.key_list.length;
        while (i_8 < _runeLen(plaintext)) {
            String ch_2 = _substr(plaintext, i_8, i_8 + 1);
            int position = index_of(((String[])(c.key_list)), ch_2);
            int new_pos = Math.floorMod((position + c.shift_key), n);
            encoded = encoded + c.key_list[new_pos];
            i_8 = i_8 + 1;
        }
        return encoded;
    }

    static String decrypt(Cipher c, String encoded_message) {
        String decoded = "";
        int i_9 = 0;
        int n_1 = c.key_list.length;
        while (i_9 < _runeLen(encoded_message)) {
            String ch_3 = _substr(encoded_message, i_9, i_9 + 1);
            int position_1 = index_of(((String[])(c.key_list)), ch_3);
            int new_pos_1 = Math.floorMod((position_1 - c.shift_key), n_1);
            if (new_pos_1 < 0) {
                new_pos_1 = new_pos_1 + n_1;
            }
            decoded = decoded + c.key_list[new_pos_1];
            i_9 = i_9 + 1;
        }
        return decoded;
    }

    static String test_end_to_end() {
        String msg = "Hello, this is a modified Caesar cipher";
        Cipher cip = new_cipher("");
        return decrypt(cip, String.valueOf(encrypt(cip, msg)));
    }
    public static void main(String[] args) {
        ssc = new_cipher("4PYIXyqeQZr44");
        encoded_1 = String.valueOf(encrypt(ssc, "Hello, this is a modified Caesar cipher"));
        System.out.println(encoded_1);
        System.out.println(decrypt(ssc, encoded_1));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
