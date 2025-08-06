public class Main {

    static java.util.Map<String,Boolean> build_set(String[] words) {
        java.util.Map<String,Boolean> m = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (String w : words) {
m.put(w, true);
        }
        return m;
    }

    static boolean word_break(String s, String[] words) {
        int n = _runeLen(s);
        java.util.Map<String,Boolean> dict = build_set(((String[])(words)));
        boolean[] dp = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i <= n) {
            dp = ((boolean[])(appendBool(dp, false)));
            i = i + 1;
        }
dp[0] = true;
        i = 1;
        while (i <= n) {
            int j = 0;
            while (j < i) {
                if (((Boolean)(dp[j]))) {
                    String sub = s.substring(j, i);
                    if (((Boolean)(dict.containsKey(sub)))) {
dp[i] = true;
                        j = i;
                    }
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return dp[n];
    }

    static void print_bool(boolean b) {
        if (((Boolean)(b))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            print_bool(word_break("applepenapple", ((String[])(new String[]{"apple", "pen"}))));
            print_bool(word_break("catsandog", ((String[])(new String[]{"cats", "dog", "sand", "and", "cat"}))));
            print_bool(word_break("cars", ((String[])(new String[]{"car", "ca", "rs"}))));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
