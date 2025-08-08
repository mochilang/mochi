public class Main {

    static String[] split_words(String s) {
        String[] words = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i+1);
            if ((ch.equals(" "))) {
                if (_runeLen(current) > 0) {
                    words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                    current = "";
                }
            } else {
                current = current + ch;
            }
            i = i + 1;
        }
        if (_runeLen(current) > 0) {
            words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        }
        return words;
    }

    static String reverse_words(String input_str) {
        String[] words_1 = ((String[])(split_words(input_str)));
        String res = "";
        int i_1 = words_1.length - 1;
        while (i_1 >= 0) {
            res = res + words_1[i_1];
            if (i_1 > 0) {
                res = res + " ";
            }
            i_1 = i_1 - 1;
        }
        return res;
    }

    static void main() {
        System.out.println(reverse_words("I love Python"));
        System.out.println(reverse_words("I     Love          Python"));
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
