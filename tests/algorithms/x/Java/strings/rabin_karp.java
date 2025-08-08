public class Main {
    static int alphabet_size;
    static int modulus;

    static int index_of_char(String s, String ch) {
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
        String digits = "0123456789";
        int idx = index_of_char(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = index_of_char(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        idx = index_of_char(digits, ch);
        if (idx >= 0) {
            return 48 + idx;
        }
        if ((ch.equals("ü"))) {
            return 252;
        }
        if ((ch.equals("Ü"))) {
            return 220;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static boolean rabin_karp(String pattern, String text) {
        int p_len = _runeLen(pattern);
        int t_len = _runeLen(text);
        if (p_len > t_len) {
            return false;
        }
        int p_hash = 0;
        int t_hash = 0;
        int modulus_power = 1;
        int i_1 = 0;
        while (i_1 < p_len) {
            p_hash = Math.floorMod((ord(pattern.substring(i_1, i_1+1)) + p_hash * alphabet_size), modulus);
            t_hash = Math.floorMod((ord(text.substring(i_1, i_1+1)) + t_hash * alphabet_size), modulus);
            if (i_1 != p_len - 1) {
                modulus_power = Math.floorMod((modulus_power * alphabet_size), modulus);
            }
            i_1 = i_1 + 1;
        }
        int j = 0;
        while (j <= t_len - p_len) {
            if (t_hash == p_hash && (_substr(text, j, j + p_len).equals(pattern))) {
                return true;
            }
            if (j == t_len - p_len) {
                j = j + 1;
                continue;
            }
            t_hash = Math.floorMod(((t_hash - ord(text.substring(j, j+1)) * modulus_power) * alphabet_size + ord(text.substring(j + p_len, j + p_len+1))), modulus);
            if (t_hash < 0) {
                t_hash = t_hash + modulus;
            }
            j = j + 1;
        }
        return false;
    }

    static void test_rabin_karp() {
        String pattern1 = "abc1abc12";
        String text1 = "alskfjaldsabc1abc1abc12k23adsfabcabc";
        String text2 = "alskfjaldsk23adsfabcabc";
        if (!(Boolean)rabin_karp(pattern1, text1) || ((Boolean)(rabin_karp(pattern1, text2)))) {
            System.out.println("Failure");
            return;
        }
        String pattern2 = "ABABX";
        String text3 = "ABABZABABYABABX";
        if (!(Boolean)rabin_karp(pattern2, text3)) {
            System.out.println("Failure");
            return;
        }
        String pattern3 = "AAAB";
        String text4 = "ABAAAAAB";
        if (!(Boolean)rabin_karp(pattern3, text4)) {
            System.out.println("Failure");
            return;
        }
        String pattern4 = "abcdabcy";
        String text5 = "abcxabcdabxabcdabcdabcy";
        if (!(Boolean)rabin_karp(pattern4, text5)) {
            System.out.println("Failure");
            return;
        }
        String pattern5 = "Lü";
        String text6 = "Lüsai";
        if (!(Boolean)rabin_karp(pattern5, text6)) {
            System.out.println("Failure");
            return;
        }
        String pattern6 = "Lue";
        if (((Boolean)(rabin_karp(pattern6, text6)))) {
            System.out.println("Failure");
            return;
        }
        System.out.println("Success.");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            alphabet_size = 256;
            modulus = 1000003;
            test_rabin_karp();
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
