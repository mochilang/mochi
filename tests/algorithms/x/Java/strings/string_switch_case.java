public class Main {

    static String[] split_words(String s) {
        String[] words = ((String[])(new String[]{}));
        String current = "";
        for (int _i = 0; _i < s.length(); _i++) {
            var ch = s.substring(_i, _i + 1);
            if (((Number)(ch)).intValue() == " ") {
                if (!(current.equals(""))) {
                    words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                    current = "";
                }
            } else {
                current = current + (String)(ch);
            }
        }
        if (!(current.equals(""))) {
            words = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        }
        return words;
    }

    static boolean is_alnum(String c) {
        return "0123456789".contains(c) || "abcdefghijklmnopqrstuvwxyz".contains(c) || "ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c) || (c.equals(" "));
    }

    static String[][] split_input(String text) {
        String[][] result = ((String[][])(new String[][]{}));
        String current_1 = "";
        for (int _i = 0; _i < text.length(); _i++) {
            var ch = text.substring(_i, _i + 1);
            if (((Boolean)(is_alnum((String)(ch))))) {
                current_1 = current_1 + (String)(ch);
            } else             if (!(current_1.equals(""))) {
                result = ((String[][])(appendObj(result, split_words(current_1))));
                current_1 = "";
            }
        }
        if (!(current_1.equals(""))) {
            result = ((String[][])(appendObj(result, split_words(current_1))));
        }
        return result;
    }

    static String capitalize(String word) {
        if (_runeLen(word) == 0) {
            return "";
        }
        if (_runeLen(word) == 1) {
            return word.toUpperCase();
        }
        return word.substring(0, 1).toUpperCase() + word.substring(1, _runeLen(word)).toLowerCase();
    }

    static String to_simple_case(String text) {
        String[][] parts = ((String[][])(split_input(text)));
        String res = "";
        for (String[] sub : parts) {
            for (String w : sub) {
                res = res + String.valueOf(capitalize(w));
            }
        }
        return res;
    }

    static String to_complex_case(String text, boolean upper_flag, String sep) {
        String[][] parts_1 = ((String[][])(split_input(text)));
        String res_1 = "";
        for (String[] sub : parts_1) {
            boolean first = true;
            for (String w : sub) {
                String word = (String)(upper_flag ? w.toUpperCase() : w.toLowerCase());
                if (((Boolean)(first))) {
                    res_1 = res_1 + word;
                    first = false;
                } else {
                    res_1 = res_1 + sep + word;
                }
            }
        }
        return res_1;
    }

    static String to_pascal_case(String text) {
        return to_simple_case(text);
    }

    static String to_camel_case(String text) {
        String s = String.valueOf(to_simple_case(text));
        if (_runeLen(s) == 0) {
            return "";
        }
        return s.substring(0, 1).toLowerCase() + s.substring(1, _runeLen(s));
    }

    static String to_snake_case(String text, boolean upper_flag) {
        return to_complex_case(text, upper_flag, "_");
    }

    static String to_kebab_case(String text, boolean upper_flag) {
        return to_complex_case(text, upper_flag, "-");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(to_pascal_case("one two 31235three4four"));
            System.out.println(to_camel_case("one two 31235three4four"));
            System.out.println(to_snake_case("one two 31235three4four", true));
            System.out.println(to_snake_case("one two 31235three4four", false));
            System.out.println(to_kebab_case("one two 31235three4four", true));
            System.out.println(to_kebab_case("one two 31235three4four", false));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
