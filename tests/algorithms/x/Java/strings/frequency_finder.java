public class Main {
    static String ETAOIN;
    static String LETTERS;

    static int etaoin_index(String letter) {
        int i = 0;
        while (i < _runeLen(ETAOIN)) {
            if ((_substr(ETAOIN, i, i + 1).equals(letter))) {
                return i;
            }
            i = i + 1;
        }
        return _runeLen(ETAOIN);
    }

    static java.util.Map<String,Integer> get_letter_count(String message) {
        java.util.Map<String,Integer> letter_count = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_1 = 0;
        while (i_1 < _runeLen(LETTERS)) {
            String c = _substr(LETTERS, i_1, i_1 + 1);
letter_count.put(c, 0);
            i_1 = i_1 + 1;
        }
        String msg = message.toUpperCase();
        int j = 0;
        while (j < _runeLen(msg)) {
            String ch = _substr(msg, j, j + 1);
            if (LETTERS.contains(ch)) {
letter_count.put(ch, (int)(((int)(letter_count).getOrDefault(ch, 0))) + 1);
            }
            j = j + 1;
        }
        return letter_count;
    }

    static String get_frequency_order(String message) {
        java.util.Map<String,Integer> letter_to_freq = get_letter_count(message);
        int max_freq = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(LETTERS)) {
            String letter = _substr(LETTERS, i_2, i_2 + 1);
            int f = (int)(((int)(letter_to_freq).getOrDefault(letter, 0)));
            if (f > max_freq) {
                max_freq = f;
            }
            i_2 = i_2 + 1;
        }
        String result = "";
        int freq = max_freq;
        while (freq >= 0) {
            String[] group = ((String[])(new String[]{}));
            int j_1 = 0;
            while (j_1 < _runeLen(LETTERS)) {
                String letter_1 = _substr(LETTERS, j_1, j_1 + 1);
                if ((int)(((int)(letter_to_freq).getOrDefault(letter_1, 0))) == freq) {
                    group = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(group), java.util.stream.Stream.of(letter_1)).toArray(String[]::new)));
                }
                j_1 = j_1 + 1;
            }
            int g_len = group.length;
            int a = 0;
            while (a < g_len) {
                int b = 0;
                while (b < g_len - a - 1) {
                    String g1 = group[b];
                    String g2 = group[b + 1];
                    int idx1 = etaoin_index(g1);
                    int idx2 = etaoin_index(g2);
                    if (idx1 < idx2) {
                        String tmp = group[b];
group[b] = group[b + 1];
group[b + 1] = tmp;
                    }
                    b = b + 1;
                }
                a = a + 1;
            }
            int g = 0;
            while (g < group.length) {
                result = result + group[g];
                g = g + 1;
            }
            freq = freq - 1;
        }
        return result;
    }

    static int english_freq_match_score(String message) {
        String freq_order = String.valueOf(get_frequency_order(message));
        String top = _substr(freq_order, 0, 6);
        String bottom = _substr(freq_order, _runeLen(freq_order) - 6, _runeLen(freq_order));
        int score = 0;
        int i_3 = 0;
        while (i_3 < 6) {
            String c_1 = _substr(ETAOIN, i_3, i_3 + 1);
            if (top.contains(c_1)) {
                score = score + 1;
            }
            i_3 = i_3 + 1;
        }
        int j_2 = _runeLen(ETAOIN) - 6;
        while (j_2 < _runeLen(ETAOIN)) {
            String c_2 = _substr(ETAOIN, j_2, j_2 + 1);
            if (bottom.contains(c_2)) {
                score = score + 1;
            }
            j_2 = j_2 + 1;
        }
        return score;
    }

    static void main() {
        System.out.println(get_frequency_order("Hello World"));
        System.out.println(english_freq_match_score("Hello World"));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ETAOIN = "ETAOINSHRDLCUMWFGYPBVKJXQZ";
            LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
