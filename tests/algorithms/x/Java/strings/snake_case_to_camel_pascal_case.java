public class Main {

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
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

    static String capitalize(String word) {
        if (_runeLen(word) == 0) {
            return "";
        }
        String first = _substr(word, 0, 1).toUpperCase();
        String rest = _substr(word, 1, _runeLen(word));
        return first + rest;
    }

    static String snake_to_camel_case(String input_str, boolean use_pascal) {
        String[] words = ((String[])(input_str.split(java.util.regex.Pattern.quote("_"))));
        String result = "";
        int index = 0;
        if (!(Boolean)use_pascal) {
            if (words.length > 0) {
                result = words[0];
                index = 1;
            }
        }
        while (index < words.length) {
            String word = words[index];
            result = result + String.valueOf(capitalize(word));
            index = index + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(snake_to_camel_case("some_random_string", false));
            System.out.println(snake_to_camel_case("some_random_string", true));
            System.out.println(snake_to_camel_case("some_random_string_with_numbers_123", false));
            System.out.println(snake_to_camel_case("some_random_string_with_numbers_123", true));
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
