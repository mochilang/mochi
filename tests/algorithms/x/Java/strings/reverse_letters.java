public class Main {

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i+1);
            if ((ch.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch;
            }
            i = i + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res;
    }

    static String join_with_space(String[] xs) {
        String s = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            s = s + xs[i_1];
            if (i_1 + 1 < xs.length) {
                s = s + " ";
            }
            i_1 = i_1 + 1;
        }
        return s;
    }

    static String reverse_str(String s) {
        String res_1 = "";
        int i_2 = _runeLen(s) - 1;
        while (i_2 >= 0) {
            res_1 = res_1 + s.substring(i_2, i_2+1);
            i_2 = i_2 - 1;
        }
        return res_1;
    }

    static String reverse_letters(String sentence, int length) {
        String[] words = ((String[])(sentence.split(java.util.regex.Pattern.quote(" "))));
        String[] result = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < words.length) {
            String word = words[i_3];
            if (_runeLen(word) > length) {
                result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(reverse_str(word))).toArray(String[]::new)));
            } else {
                result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(word)).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return join_with_space(((String[])(result)));
    }

    static void test_reverse_letters() {
        if (!(reverse_letters("Hey wollef sroirraw", 3).equals("Hey fellow warriors"))) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        if (!(reverse_letters("nohtyP is nohtyP", 2).equals("Python is Python"))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (!(reverse_letters("1 12 123 1234 54321 654321", 0).equals("1 21 321 4321 12345 123456"))) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        if (!(reverse_letters("racecar", 0).equals("racecar"))) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
    }

    static void main() {
        test_reverse_letters();
        System.out.println(reverse_letters("Hey wollef sroirraw", 3));
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
