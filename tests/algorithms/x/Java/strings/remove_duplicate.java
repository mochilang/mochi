public class Main {

    static String[] split_ws(String s) {
        String[] res = ((String[])(new String[]{}));
        String word = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals(" "))) {
                if (!(word.equals(""))) {
                    res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(word)).toArray(String[]::new)));
                    word = "";
                }
            } else {
                word = word + ch;
            }
            i = i + 1;
        }
        if (!(word.equals(""))) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(word)).toArray(String[]::new)));
        }
        return res;
    }

    static boolean contains(String[] xs, String x) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[i_1].equals(x))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String[] unique(String[] xs) {
        String[] res_1 = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < xs.length) {
            String w = xs[i_2];
            if (!(Boolean)contains(((String[])(res_1)), w)) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(w)).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] insertion_sort(String[] arr) {
        String[] a = ((String[])(arr));
        int i_3 = 1;
        while (i_3 < a.length) {
            String key = a[i_3];
            int j = i_3 - 1;
            while (j >= 0 && (a[j].compareTo(key) > 0)) {
a[j + 1] = a[j];
                j = j - 1;
            }
a[j + 1] = key;
            i_3 = i_3 + 1;
        }
        return a;
    }

    static String join_with_space(String[] xs) {
        String s = "";
        int i_4 = 0;
        while (i_4 < xs.length) {
            if (i_4 > 0) {
                s = s + " ";
            }
            s = s + xs[i_4];
            i_4 = i_4 + 1;
        }
        return s;
    }

    static String remove_duplicates(String sentence) {
        String[] words = ((String[])(split_ws(sentence)));
        String[] uniq = ((String[])(unique(((String[])(words)))));
        String[] sorted_words = ((String[])(insertion_sort(((String[])(uniq)))));
        return join_with_space(((String[])(sorted_words)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(remove_duplicates("Python is great and Java is also great"));
            System.out.println(remove_duplicates("Python   is      great and Java is also great"));
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
